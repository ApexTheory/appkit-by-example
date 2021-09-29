package freezecoin

import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}

import java.util.Collections

object FreezeCoin {

  def sendTx(configFileName: String): String = {

    // Node configuration values
    val conf: ErgoToolConfig = ErgoToolConfig.load(configFileName)
    val nodeConf: ErgoNodeConfig = conf.getNode
    val explorerUrl: String = RestApiErgoClient.getDefaultExplorerUrl(NetworkType.TESTNET)

    // Fetch parameters from config
    val newBoxSpendingDelay: Int = conf.getParameters.get("newBoxSpendingDelay").toInt
    val ownerAddress: Address = Address.create(conf.getParameters.get("ownerAddress"))
    val addressIndex: Int = conf.getParameters.get("addressIndex").toInt

    // Create ErgoClient instance (represents connection to node)
    val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConf, explorerUrl)

    // Execute transaction
    val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {

      // Initialize prover (signs the transaction)
      val prover: ErgoProver = ctx.newProverBuilder
        .withMnemonic(
          SecretString.create(nodeConf.getWallet.getMnemonic),
          SecretString.create(nodeConf.getWallet.getPassword))
        .withEip3Secret(addressIndex)
        .build()

      // Get input (spending) boxes from node wallet
      val wallet: ErgoWallet = ctx.getWallet
      val amountToSend: Long = Parameters.OneErg
      val totalToSpend: Long = amountToSend + Parameters.MinFee
      val boxes: java.util.Optional[java.util.List[InputBox]] = wallet.getUnspentBoxes(totalToSpend)
      if (!boxes.isPresent)
        throw new ErgoClientException(s"Not enough coins in the wallet to pay $totalToSpend", null)

      // Define protection script
      val freezeCoinScript: String = s"""
        { sigmaProp(HEIGHT > freezeDeadline) && ownerPk }
        """.stripMargin

      // Compile contract (with parameter substitution)
      val contract: ErgoContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("freezeDeadline", ctx.getHeight + newBoxSpendingDelay)
          .item("ownerPk", ownerAddress.getPublicKey)
          .build(),
        freezeCoinScript)

      // Create signed transaction
      val signed: SignedTransaction = BoxOperations.putToContractTx(
        ctx,
        prover,
        true,
        contract,
        amountToSend,
        Collections.emptyList()
      )

      // Submit transaction to node
      val txId: String = ctx.sendTransaction(signed)

      // Return transaction as JSON string
      signed.toJson(true)
    })
    txJson
  }

  def main(args: Array[String]): Unit = {
    val txJson: String = sendTx("ergo_config.json")
    System.out.println(txJson)
  }

}