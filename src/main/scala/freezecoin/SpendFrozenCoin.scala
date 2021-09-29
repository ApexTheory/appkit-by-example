package freezecoin

import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}

object SpendFrozenCoin {

  def sendTx(configFileName: String): String = {

    // Node configuration values
    val conf: ErgoToolConfig = ErgoToolConfig.load(configFileName)
    val nodeConf: ErgoNodeConfig = conf.getNode
    val explorerUrl: String = RestApiErgoClient.getDefaultExplorerUrl(NetworkType.TESTNET)

    // Fetch parameters from config
    val ownerAddress: Address = Address.create(conf.getParameters.get("ownerAddress"))
    val addressIndex: Int = conf.getParameters.get("addressIndex").toInt
    val freezeAddress: String = conf.getParameters.get("freezeAddress")

    // Create ErgoClient instance (represents connection to node)
    val ergoClient: ErgoClient = RestApiErgoClient.create(nodeConf, explorerUrl)

    // Execute transaction
    val txJson: String = ergoClient.execute((ctx: BlockchainContext) => {

      // Initialize prover (signs the transaction)
      val prover = ctx.newProverBuilder
        .withMnemonic(
          SecretString.create(nodeConf.getWallet.getMnemonic),
          SecretString.create(nodeConf.getWallet.getPassword))
        .withEip3Secret(addressIndex)
        .build()

      // Get input (spending) box from FreezeCoin contract address
      val spendingAddress: Address = Address.create(freezeAddress)
      val spendingBoxes: java.util.List[InputBox] = ctx.getUnspentBoxesFor(spendingAddress, 0, 20)

      // Frozen amount to retrieve (remember to account for transaction fee)
      val amountToSend: Long = Parameters.OneErg - Parameters.MinFee

      // Create signed transaction
      val signed: SignedTransaction = BoxOperations.spendBoxesTx(
        ctx,
        ctx.newTxBuilder,
        spendingBoxes,
        prover,
        ownerAddress,
        amountToSend,
        Parameters.MinFee)

      // Submit transaction to node
      val txId: String = ctx.sendTransaction(signed)

      // Return transaction as JSON string
      signed.toJson(true)
    })
    txJson
  }

  def main(args: Array[String]): Unit = {
    val txJson = sendTx("ergo_config.json")
    System.out.println(txJson)
  }

}