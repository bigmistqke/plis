import readline from "readline"
import { evaluate, parse, standardEnvironment, tokenize } from "../src" // Adjust the import path as necessary

const rl = readline.createInterface({
  input: process.stdin,
  output: process.stdout,
  prompt: "plis> ",
})

const env = standardEnvironment()

function repl(prompt: string): void {
  rl.prompt()

  rl.on("line", (line: string) => {
    try {
      if (line.trim() === "quit") {
        // Allows exiting the REPL
        rl.close()
        return
      }

      const tokens = tokenize(line)
      const ast = parse(tokens)
      const result = evaluate(ast, env)

      console.log(result) // Output the result of evaluation
    } catch (e: any) {
      // Catch and display errors
      console.error("Error:", e.message)
    }
    rl.prompt()
  }).on("close", () => {
    console.log("Goodbye!")
    process.exit(0)
  })
}

repl("plis> ") // Start the REPL with the prompt
