package kalahari


import org.boris.xlloop.FunctionServer
import org.boris.xlloop.handler._
import org.boris.xlloop.reflect._

class ExcelBridge

object ExcelBridge extends Application{
        // Create function server on the default port
        val fs = new FunctionServer()

        // Create a reflection function handler and add the Math methods
        val rfh = new ReflectFunctionHandler()
        rfh.addMethods("Kalahari.", classOf[Kalahari])

        // Create a function information handler to register our functions
        val firh = new FunctionInformationHandler()
        firh.add(rfh.getFunctions())

        // Set the handlers
        val cfh = new CompositeFunctionHandler()
        cfh.add(rfh)
        cfh.add(firh)
        fs.setFunctionHandler(new DebugFunctionHandler(cfh))

        // Run the engine
        System.out.println("Listening on port " + fs.getPort() + "...")
        fs.run()
}


