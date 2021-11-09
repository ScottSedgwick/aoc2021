require 'rake'

currentDay = '01'

task :run do
  run currentDay
end

task :test do
  test currentDay
end

def run(day)
  sh "cabal build advent-#{day}"
  sh "cabal exec advent-#{day}"
end

def test(day)
  sh "cabal test advent-test-#{day}"
end