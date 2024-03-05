package Common
/*========================================================
 * Author : xie-1399
 * language: SpinalHDL v1.9.4
 * date 2024.1.24
 * some common tools build with the spinal for using
 *========================================================
 */


import spinal.core._
import spinal.lib._
import spinal.lib.tools._

object SpinalTools{

  class PrefixComponent() extends Component{
    noIoPrefix()
  }

  def genDiagram = HDElkDiagramGen  /* gen the simple diagram */

}
