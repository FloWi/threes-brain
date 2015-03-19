package threesbrain.neuralnetwork

import java.io.FileWriter
import java.util.concurrent.TimeUnit.NANOSECONDS

import org.apache.log4j.Logger

import scala.annotation.tailrec
import scala.collection.GenSeq
import scala.util.Random

object GeneticAlgorithm {
    val mutationRate = 0.1
    val maxMutationPerturbation = 0.3
    val crossOverRate = 0.7
    val eliteProportion = 0.1
    val populationSize = 1000
    val numGenerations = 5000
    
    type Genome = List[Double]

  val logger = Logger.getLogger(this.getClass)

    def train(scoreFun: (NeuralNetwork) => Double,
              layerSizes: List[Int]): NeuralNetwork = {
        assert(populationSize > 0)

        val timeStart = System.nanoTime()
        val numWeights = NeuralNetwork.weightLengths(layerSizes).sum
        
        val fileName = s"threesbrain-log-${System.currentTimeMillis()/1000}.csv"
        val csvFile = new FileWriter(fileName)
        csvFile.write("Epoch,PopBestAvg,PopWorstAvg,PopAvg\n")
        logger.debug("Epoch        Best       Worst    Average")

        def epoch(population: List[Genome], n: Int): List[Genome] = {
          val epochStart = System.currentTimeMillis()
          //val par: ParSeq[Genome] = population.par
          //par.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(30))
          //par.tasksupport = new ThreadPoolTaskSupport(new ThreadPoolExecutor(20, 20, 1l, TimeUnit.MINUTES, new ArrayBlockingQueue(10000)))

          //println(s"size: ${population.size}")
          val scores = population.zipWithIndex.map { case(genome, index) => {
            val start = System.currentTimeMillis()
            //logger.debug(s"starting $index")
            val result: Double = scoreFun(NeuralNetwork.fromWeights(layerSizes, genome))
            val stop = System.currentTimeMillis()
            val duration = stop - start
            //logger.debug(s"stopping $index, result: $result, duration: ${duration}ms")
            result
          }}

          val epochEnd = System.currentTimeMillis()
          val epochDuration = epochEnd - epochStart
          logger.debug(s"Epoch duration: ${epochDuration}ms")


          val (max, min, avg) = (scores.max, scores.min, scores.sum/scores.length)
            csvFile.write(s"$max,$min,$avg\n")
            csvFile.flush

            logger.debug(s"${numGenerations - n + 1}/$numGenerations".padTo(8, ' ') + f"$max%10.2f,$min%10.2f,$avg%10.2f")

            // Mutate single weights according to mutation rate
            def mutate(genome: Genome) = genome.map({ w =>
                if (Random.nextDouble() < mutationRate)
                    w + (Random.nextDouble() * 2.0 - 1.0) * maxMutationPerturbation
                else
                    w
            })

            // Create two children and cross over their genomes if the cross-over random variable is active
            def crossover(mom: Genome, dad: Genome) = {
                def onePoint() = {
                    val crossoverPoint = Random.nextInt(mom.length - 2) + 1
                    List(
                        mom.take(crossoverPoint) ::: dad.drop(crossoverPoint),
                        dad.take(crossoverPoint) ::: mom.drop(crossoverPoint)
                    )
                }
                def twoPoint() = {
                    val p1 = Random.nextInt(mom.length - 2) + 1
                    val p2 = (Random.nextInt(mom.length - 3) + 1) match {
                        case p if p >= p1 => p + 1
                        case p => p
                    }
                    val crossoverPoint1 = Math.min(p1, p2)
                    val crossoverPoint2 = Math.max(p1, p2)
                    List(
                        mom.take(crossoverPoint1) ::: dad.drop(crossoverPoint1).take(crossoverPoint2 - crossoverPoint1) ::: mom.drop(crossoverPoint2),
                        dad.take(crossoverPoint1) ::: mom.drop(crossoverPoint1).take(crossoverPoint2 - crossoverPoint1) ::: dad.drop(crossoverPoint2)
                    )
                }
                def uniform() = {
                    val flips = List.fill(numWeights)(Random.nextDouble() > 0.7)
                    val momDadFlips = mom.zip(dad).zip(flips)
                    List(
                        momDadFlips.map{case ((m, d), flip) => if (flip) m else d},
                        momDadFlips.map{case ((m, d), flip) => if (flip) d else m}
                    )
                }
                def identity() = {
                    List(mom, dad)
                }
                val crossoverFunc = twoPoint _  // it seems that having no crossover is more efficient
                
                if (Random.nextDouble() < crossOverRate)
                    crossoverFunc()
                else
                    identity()
            }

            // Roulette-wheel selection
            def pickParent() = {
                def pick(pop: List[Genome], scores: GenSeq[Double], num: Double): Genome = {
                    if (num < scores.head) pop.head
                    else pick(pop.tail, scores.tail, num - scores.head)
                }
                pick(population, scores, Random.nextInt(scores.sum.toInt))
            }

            def makeOffsprings() = {
                crossover(pickParent(), pickParent()).map(mutate)
            }

            List.fill(population.size / 2)(makeOffsprings()).flatten
        }

        @tailrec
        def trainRec(population: List[Genome], cyclesLeft: Int): List[Genome] = cyclesLeft match {
            case 0 => population
            case n =>
                //print(s"${numGenerations - n + 1}/$numGenerations".padTo(8, ' '))
                csvFile.write(numGenerations - n + 1+",")
                trainRec(epoch(population, n), n - 1)
        }

        def randomGenome() = List.fill(numWeights)(Random.nextDouble() * 2.0 - 1.0)
        val startPopulation = List.fill(populationSize)(randomGenome())

        val weights = trainRec(startPopulation, numGenerations).maxBy(
            genome => scoreFun(NeuralNetwork.fromWeights(layerSizes, genome))
        )
        csvFile.close

        val timeEnd = System.nanoTime()
        val timeDelta = timeEnd - timeStart
        
        println("Total training time: " +
                NANOSECONDS.toHours(timeDelta) + "h " + 
                NANOSECONDS.toMinutes(timeDelta) % 60 + "m " + 
                NANOSECONDS.toSeconds(timeDelta) % 60 + "s")

        println("weights: " + weights.mkString(", "))

        NeuralNetwork.fromWeights(layerSizes, weights)
    }
}
