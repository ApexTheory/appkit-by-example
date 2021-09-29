package pinlock

import org.ergoplatform.appkit._
import org.ergoplatform.appkit.config.{ErgoNodeConfig, ErgoToolConfig}
import org.ergoplatform.appkit.impl.ErgoTreeContract

import java.util.stream.Collectors

object UnlockCoin {

  def sendTx(configFileName: String): String = {

    // Node configuration values
    val conf: ErgoToolConfig = ErgoToolConfig.load(configFileName)
    val nodeConf: ErgoNodeConfig = conf.getNode
    val explorerUrl: String = RestApiErgoClient.getDefaultExplorerUrl(NetworkType.TESTNET)

    // Fetch parameters from config
    val ownerAddress: Address = Address.create(conf.getParameters.get("ownerAddress"))
    val addressIndex: Int = conf.getParameters.get("addressIndex").toInt
    val pinLockAddress: String = conf.getParameters.get("pinLockAddress")
    val pinLockBoxId: String = conf.getParameters.get("pinLockBoxId")
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

      // Get input (spending) box from Pin Lock contract address
      val spendingAddress: Address = Address.create(pinLockAddress)
      val spendingBoxes: java.util.List[InputBox] = ctx.getUnspentBoxesFor(spendingAddress, 0, 20)
        .stream()
        .filter(_.getId == ErgoId.create(pinLockBoxId))
        .collect(Collectors.toList())

      // Frozen amount to retrieve (remember to account for transaction fee)
      val amountToSend: Long = Parameters.OneErg - Parameters.MinFee

      // Create unsigned transaction builder
      val txB = ctx.newTxBuilder

      // Create output box (spend pin-locked coins)
      val newBox = txB.outBoxBuilder
        .value(amountToSend)
        .contract(
          new ErgoTreeContract(ownerAddress.getErgoAddress.script))
        .registers(ErgoValue.of(pin.getBytes))
        .build()

      // Create unsigned transaction
      val tx = txB.boxesToSpend(spendingBoxes)
        .outputs(newBox)
        .fee(Parameters.MinFee)
        .sendChangeTo(prover.getP2PKAddress)
        .build()

      // Sign transaction with prover
      val signed = prover.sign(tx)

      // Submit transaction to node
      val txId = ctx.sendTransaction(signed)

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