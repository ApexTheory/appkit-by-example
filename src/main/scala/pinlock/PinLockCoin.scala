package pinlock

import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}
import scorex.crypto.hash.Blake2b256

object PinLockCoin {

  def sendTx(configFileName: String): String = {

    // Node configuration values
    val conf: ErgoToolConfig = ErgoToolConfig.load(configFileName)
    val nodeConf: ErgoNodeConfig = conf.getNode
    val explorerUrl: String = RestApiErgoClient.getDefaultExplorerUrl(NetworkType.TESTNET)

    // Fetch parameters from config
    val addressIndex: Int = conf.getParameters.get("addressIndex").toInt
    val pin: String = conf.getParameters.get("pin")

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
      val pinLockScript: String = s"""
        sigmaProp(INPUTS(0).R4[Coll[Byte]].get == blake2b256(OUTPUTS(0).R4[Coll[Byte]].get))
        """.stripMargin

      // Create unsigned transaction builder
      val txB: UnsignedTransactionBuilder = ctx.newTxBuilder

      // Create output (pin-locked) box
      val newBox: OutBox = txB.outBoxBuilder
        .value(amountToSend)
        .contract(
          ctx.compileContract(
            ConstantsBuilder.empty(),
            pinLockScript))
        .registers(ErgoValue.of(Blake2b256.hash(pin)))
        .build()

      // Create unsigned transaction
      val tx: UnsignedTransaction = txB
        .boxesToSpend(boxes.get)
        .outputs(newBox)
        .fee(Parameters.MinFee)
        .sendChangeTo(prover.getP2PKAddress)
        .build()

      // Sign transaction with prover
      val signed: SignedTransaction = prover.sign(tx)

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