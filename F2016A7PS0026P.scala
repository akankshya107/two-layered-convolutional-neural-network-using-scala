package pplAssignment

object F2016A7PS0026P{
    //Start Coding from here

    //first N elements of list
    def firstN[A](list:List[A], N:Int):List[A] = {
        list match {
            case Nil => Nil
            case _ if(N==0) => Nil
            case x::xs => List(x) ::: firstN(xs, N-1)
        }
    }

    //drop first n elements from list
    def dropFirstN[A](list:List[A], N:Int):List[A] = {
  	    list match{
  		    case Nil => Nil
            case _ if(N==0) => list
  		    case x::xs => dropFirstN(xs, N-1)
  	    }
    }

    //drop first K elements in each component list up til N rows
    def modifyList[A](Image:List[List[A]], K:Int, N:Int):List[List[A]] = {
        if(N==0) Nil
        else {
            if(Image.nonEmpty) {
                val l = dropFirstN(Image.head, K)
                l match{
                    case Nil => Nil
                    case _ => List(l) ::: modifyList(Image.tail, K, N-1)
                }
            }
            else Nil
        }
    }
    
    //Convert list of lists of length N into a list
    def ListConcat[A](list:List[List[A]], N:Int):List[A] = {
  	    if(list.nonEmpty){
            val l = firstN(list.head, N)
            l match{
                case Nil => Nil
                case _ => l ::: ListConcat(list.tail, N)
            }
  	    }
        else Nil
    }

    //find max element in a list
    def max(list: List[Double]): Double = {
        if(list.tail.nonEmpty){
            val tl = max(list.tail)
            if(tl > list.head) tl
            else list.head
        }
        else list.head
    }

    //find min element in a list
    def min(list: List[Double]): Double = {
        if(list.tail.nonEmpty){
            val tl = min(list.tail)
            if(tl < list.head) tl
            else list.head
        }
        else list.head
    }

    //get average of list elements
    def average(list: List[Double]): Double = {
        def sum(list: List[Double]): Double = {
            list match {
                case Nil => 0
                case x::xs => x+sum(xs)
            }
        }
        list match {
            case Nil => 0
            case _ => sum(list)/list.length
        }
    }

    def dotProduct(matrix_1: List[List[Double]], matrix_2: List[List[Double]]):Double = {
        //dot product row by row
        def dotProductRow(matrix_1_row:List[Double], matrix_2_row:List[Double]):Double = {
            if(matrix_1_row.nonEmpty && matrix_2_row.nonEmpty){
                matrix_1_row.head*matrix_2_row.head + dotProductRow(matrix_1_row.tail, matrix_2_row.tail)
            }
            else 0
        }
        if(matrix_1.nonEmpty && matrix_2.nonEmpty){
            dotProductRow(matrix_1.head, matrix_2.head) + dotProduct(matrix_1.tail, matrix_2.tail)
        }
        else 0
    }

    def convolute(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int]): List[List[Double]] = {
        //first row of convoluted output
        def convoluteRow(ImageRow:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int]):List[Double] = {
            if(imageSize.tail.head>=kernelSize.tail.head){
                List(dotProduct(ImageRow, Kernel)) ::: convoluteRow(modifyList(ImageRow, 1, kernelSize.head), Kernel, List(imageSize.head, imageSize.tail.head-1), kernelSize)
            }
            else Nil
        }
        if(imageSize.head>=kernelSize.head){
            List(convoluteRow(Image, Kernel, imageSize, kernelSize)) ::: convolute(Image.tail, Kernel, List(imageSize.head-1, imageSize.tail.head), kernelSize)
        }
        else Nil
    }

    def activationLayer(activationFunc:Double=>Double, Image:List[List[Double]]):List[List[Double]] = {
        //activation applied row by row
        def activateRow(activationFunc:Double=>Double, ImageRow:List[Double]):List[Double] = {
            if(ImageRow.nonEmpty){
                List(activationFunc(ImageRow.head)) ::: activateRow(activationFunc, ImageRow.tail)
            }
            else Nil
        }
        if(Image.nonEmpty){
            List(activateRow(activationFunc, Image.head)) ::: activationLayer(activationFunc, Image.tail)
        }
        else Nil
    }
 
    def singlePooling(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int) : List[Double] = {
        if(Image.nonEmpty){
            List(poolingFunc(ListConcat(Image, K))) ::: singlePooling(poolingFunc, modifyList(Image, K, K), K)
        }
        else Nil
    }

    def poolingLayer(poolingFunc:List[Double]=>Double, Image:List[List[Double]], K:Int): List[List[Double]] = {
        if(Image.nonEmpty){
            List(singlePooling(poolingFunc, firstN(Image, K), K)) ::: poolingLayer(poolingFunc, dropFirstN(Image, K), K)
        }
        else Nil
    }

    def mixedLayer(Image:List[List[Double]], Kernel: List[List[Double]], imageSize:List[Int], kernelSize: List[Int], activationFunc:Double=>Double, poolingFunc:List[Double]=>Double, K:Int):List[List[Double]] = {
        poolingLayer(poolingFunc, activationLayer(activationFunc, convolute(Image, Kernel, imageSize, kernelSize)), K) //final dimensions of output matrix: (imageSize.head-(imageSize.head%kernelSize.head))/K, (imageSize.tail.head-(imageSize.tail.head%kernelSize.tail.head))/K
    }

    def normalise(Image:List[List[Double]]):List[List[Int]] = {
        //min-max normalization
        def min_max_normalise(Image:List[List[Double]], minList:Double, maxList:Double):List[List[Int]] = {
            //min-max normalization row by row
            def min_max_normaliseRow(ImageRow:List[Double], minList:Double, maxList:Double):List[Int] = {
                if(ImageRow.nonEmpty){
                    List(Math.round(((ImageRow.head-minList)*255)/(maxList-minList)).toInt) ::: min_max_normaliseRow(ImageRow.tail, minList, maxList)
                }
                else Nil
            }
            if(Image.nonEmpty){
                List(min_max_normaliseRow(Image.head, minList, maxList)) ::: min_max_normalise(Image.tail, minList, maxList)
            }
            else Nil
        }
        min_max_normalise(Image, min(ListConcat(Image, Image.head.length)), max(ListConcat(Image, Image.head.length)))
    }

    def assembly(Image:List[List[Double]], imageSize:List[Int], w1:Double, w2:Double, b:Double, Kernel1:List[List[Double]], kernelSize1:List[Int], Kernel2:List[List[Double]], kernelSize2:List[Int], Kernel3:List[List[Double]], kernelSize3:List[Int], Size:Int):List[List[Int]] = {
         //apply function on each element of list
        def modifyElements(func:Double=>Double, Image:List[List[Double]]):List[List[Double]] = {
            //apply function row by row
            def modifyElementsRow(func:Double=>Double, ImageRow:List[Double]):List[Double] = {
                if(ImageRow.nonEmpty){
                    List(func(ImageRow.head)) ::: modifyElementsRow(func, ImageRow.tail)
                }
                else Nil
            }
            if(Image.nonEmpty){
                List(modifyElementsRow(func, Image.head)) ::: modifyElements(func, Image.tail)
            }
            else Nil
        }
        //add elements in two lists 
        def addList(list1:List[List[Double]], list2:List[List[Double]]):List[List[Double]] = {
            //add lists row by row
            def addListRow(list1:List[Double], list2:List[Double]):List[Double] = {
                if(list1.nonEmpty && list2.nonEmpty){
                    List(list1.head + list2.head) ::: addListRow(list1.tail, list2.tail)
                }
                else Nil
            }
            if(list1.nonEmpty && list2.nonEmpty){
                List(addListRow(list1.head, list2.head)) ::: addList(list1.tail, list2.tail)
            }
            else Nil
        }
        normalise(mixedLayer(modifyElements((x:Double)=>x+b, addList(modifyElements((x:Double)=> w1*x, mixedLayer(Image, Kernel1, imageSize, kernelSize1, (x:Double)=> if(x>0) x else 0, average, Size)) , modifyElements((x:Double)=> w2*x, mixedLayer(Image, Kernel2, imageSize, kernelSize2, (x:Double)=> if(x>0) x else 0, average, Size)))), Kernel3, List((imageSize.head-(imageSize.head%kernelSize1.head))/Size, (imageSize.tail.head-(imageSize.tail.head%kernelSize1.tail.head))/Size), kernelSize3, (x:Double)=> if(x>0) x else 0.5*x, max, Size))
    }
}