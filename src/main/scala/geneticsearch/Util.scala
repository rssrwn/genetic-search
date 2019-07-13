package geneticsearch

import scala.util.Random


object Util {

    /*
    Randomly pick <numToPick> numbers from <nums>
    numToPick must be less than length of nums
     */
    def randPick(nums: Vector[Int], numToPick: Int): Vector[Int] = {
        assert(numToPick <= nums.length, "")
        randPickRec(nums, Nil, numToPick)
    }

    private def randPickRec(nums: Vector[Int], currPicks: List[Int], numToPick: Int): Vector[Int] = {
        if (numToPick == currPicks.length) {
            currPicks.toVector
        } else {
            val rand = Random.nextInt(nums.length)
            val idx = nums(rand)
            val newIdxs = nums.diff(Seq(rand))
            randPickRec(newIdxs, idx :: currPicks, numToPick)
        }
    }

}
