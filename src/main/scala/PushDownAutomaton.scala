import scala.collection.mutable

class PushDownAutomaton(val alphabet: mutable.HashSet[String], instructions: Seq[String]) {
  private val stack = mutable.Stack[String]()
  private var stackAlphabet = mutable.HashSet[String]()

  private val states = mutable.HashSet[String]()
  private final val InitialState: String = "s0"
  private final val FinalStates = mutable.HashSet[String]("s0")
  private val transitions = mutable.HashSet[TransitionFunction]()
  private final val LowestStackPoint = "h0"

  states.add(InitialState)
  stackAlphabet = alphabet
  stackAlphabet.add(LowestStackPoint)

  instructions.foreach(parseLine)
  transitions.add(TransitionFunction(InitialState, "~", LowestStackPoint, InitialState, "~"))

  transitions.addAll(alphabet.map(letter => TransitionFunction(InitialState, letter, letter, InitialState, "~")))

  private def parseLine(line: String): Unit = {
    val leftPart = line.substring(0, line.indexOf('>'))
    stackAlphabet.add(leftPart)

    val rightParts = line.substring(line.indexOf('>') + 1, line.length - leftPart.length - 1).split('|')
    rightParts.foreach(rightPart => {
      val reversed = rightPart.reverse
      transitions.add(TransitionFunction(InitialState, "~", leftPart, InitialState, reversed))
    })
  }

  def isExecutable(input: String, stackSource: Array[String]): Either[String, Unit] = {
    val configurationQueue = mutable.Queue[Configuration]()
    stackSource.foreach(stack.push)
    configurationQueue.enqueue(Configuration(InitialState, input, stack, None))

    while (configurationQueue.nonEmpty) {
      val currentConfig = configurationQueue.dequeue()
      if (FinalStates.contains(currentConfig.state) && currentConfig.stack.isEmpty && currentConfig.input.isEmpty)
        return Right(())
      if (currentConfig.stack.nonEmpty) {
        if (!(currentConfig.input.isEmpty && currentConfig.stack.size != 1)) {
          val currentSymbolFromStack = currentConfig.stack.pop()
          val firstInputCharacter = currentConfig.input(0).toString
          if (!stackAlphabet.contains(firstInputCharacter)) {
            return Left(s"Bad symbol $firstInputCharacter")
          }
          val transitionsFnWithEmptySymbols = transitions.filter(tf => tf.currentState == currentConfig.state &&
            tf.stackSymbol == currentSymbolFromStack && tf.inputSymbol == "~"
          )
          if (transitionsFnWithEmptySymbols.nonEmpty) {
            transitionsFnWithEmptySymbols.foreach(fn => configurationQueue.enqueue(getNextConfiguration(fn, currentConfig)))
          } else {
            transitions.foreach(fn => {
              val sym = currentConfig.input(0).toString
              if (fn.currentState == currentConfig.state && fn.inputSymbol == sym && fn.stackSymbol == currentSymbolFromStack) {
                configurationQueue.enqueue(getNextConfiguration(fn, currentConfig))
              }
            })
          }
        }
      }
    }
    Left("Automate is invalid since either stack is filled or input line has some symbols")
  }

  private def getNextConfiguration(function: TransitionFunction, previous: Configuration): Configuration = {
    val temporaryStack = previous.stack.clone()
    function.leavingStackElements.filter(_!='~').foreach(element => temporaryStack.push(element.toString))
    var line = previous.input
    if (function.inputSymbol != "~") {
      line = line.substring(1)
    }
    Configuration(function.nextState, line, temporaryStack, Some(previous))
  }
}
