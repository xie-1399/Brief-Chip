package VLSI

/* =======================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.2.27
 * the PE FSM used to control the PE Array State
 * =======================================================
 */


import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class PE_FSM extends Component {

  val io = new Bundle{
    val start_conv = in Bool()
    val start_again = in Bool()


    val ifm_read = out Bool()
    val wgt_read = out Bool()
    val p_valid_out = out Bool()
    val last_channel_out = out Bool()
    val end_conv = out Bool()

  }

  // val




  val fsm = new StateMachine{

  }

}
