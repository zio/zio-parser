package zio.parser.target

trait Target[Output] {
  type Capture

  def write(value: Output): Unit

  def capture(): Capture
  def emit(capture: Capture): Unit
  def drop(capture: Capture): Unit
}
