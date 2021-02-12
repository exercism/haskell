# Hints

Your code should contain a frequency :: Int -> [Text] -> Map Char Int
function which accepts a number of workers to use in parallel and a list
of texts and returns the total frequency of each letter in the text.

## Benchmark

Check how changing number of workers affects performance of your solution by running the benchmark. Use `stack bench` to run it. Feel free to modify `bench/Benchmark.hs` to explore your solution's performance on different inputs.
