package ScalaImages

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import java.awt.{Color, color}
import javax.swing.ImageIcon
import javax.swing.JFrame
import javax.swing.JLabel
import java.awt.FlowLayout
import scalation.linalgebra.{MatrixI, VectorI}
import scala.math.abs

object imageObject {

  /*def getBuffImage() returns the buffered image object on the path.*/
  def getBuffImage(path: String): java.awt.image.BufferedImage = {
    val image = new File(path)
    ImageIO.read(image)
  }

  /*def getRgbArray method returns a array of RGB values of size image.width * image.height*/
  def getRbgMatrix(buffImage: BufferedImage): MatrixI = {
    val image = buffImage
    var pixels = new Array[Int](image.getWidth*image.getHeight)
    toMatrix(image.getRGB(0,0,image.getWidth,image.getHeight,pixels,0,image.getWidth), image.getHeight,image.getWidth)
  }

  /*def getRed method returns vectorI with values red channel from the image*/
  def getRed(rgbMatrix: MatrixI): scalation.linalgebra.MatrixI = {
    val rgb = rgbMatrix.apply()
    var red = rgb.map(arr => arr.map(x => new Color(x).getRed))
    new MatrixI(red)
  }

  /*def getGreen method returns vectorI with values green channel from the image*/
  def getGreen(rgbMatrix: MatrixI): scalation.linalgebra.MatrixI = {
    val rgb = rgbMatrix.apply()
    var green = rgb.map(arr => arr.map(x => new Color(x).getGreen))
    new MatrixI(green)
  }

  /*def getBlue method returns vectorI with values blue channel from the image*/
  def getBlue(rgbMatrix: MatrixI): scalation.linalgebra.MatrixI = {
    val rgb = rgbMatrix.apply()
    var blue = rgb.map(arr => arr.map(x => new Color(x).getBlue))
    new MatrixI(blue)
  }

  /*def getAlpha method returns vectorI with values alpha channel from the image*/
  def getAlpha(rgbMatrix: MatrixI): scalation.linalgebra.MatrixI = {
    val rgb = rgbMatrix.apply()
    var alpha = rgb.map(arr => arr.map(x => new Color(x).getAlpha))
    new MatrixI(alpha)
  }

  /*Returns height of the image*/
  def height(bufferedImage: BufferedImage): Int = {
    bufferedImage.getHeight
  }

  /*Returns width of the image*/
  def width(bufferedImage: BufferedImage): Int = {
    bufferedImage.getWidth
  }

  /*Converts the image into a grey scale image and return MatrixI object*/
  def toGrey(rbgMatrix: MatrixI): BufferedImage = {
    var red = getRed(rbgMatrix).*=(21)./(100)
    var blue = getBlue(rbgMatrix).*=(8)./(100)
    var green = getGreen(rbgMatrix).*=(71)./(100)
    val bwarr = red.+(green).+(blue)
    toBuffImage(bwarr)
  }

  /*Converts a vector to matrix*/
  def toMatrix(vec: VectorI,height: Int,width: Int): MatrixI = {
    var matI = new MatrixI(height,width)
    for ( i <- (0 until height))
      matI.set(i, vec.apply(i * width until (i + 1) * width))
    matI
  }

  /*Converts a array to matrix*/
  def toMatrix(arr: Array[Int],height: Int,width: Int): MatrixI = {
    var x = new MatrixI(height,width)
    for ( i <- 0 until height)
      x(i) = VectorI(arr.slice(i * width,(i + 1) * width))
    x
  }

  /*Converts a given matrixI to buffered image*/
  def toBuffImage(mat: MatrixI): BufferedImage = {
    var resBuffImage = new BufferedImage(mat.dim2,mat.dim1,BufferedImage.TYPE_INT_RGB)
    for (i <- (0 until mat.dim1))
      for (j <- (0 until mat.dim2)){
        var rbg = mat.apply(i,j)
        var color = new Color(rbg)
        resBuffImage.setRGB(j,i,color.getRGB)
      }
    resBuffImage
  }

   /*Converts a given matrixI to buffered image*/
  def toBuffImage(red: MatrixI,green: MatrixI,blue: MatrixI): BufferedImage = {
    var resBuffImage = new BufferedImage(red.dim2,red.dim1,BufferedImage.TYPE_INT_RGB)
    for (i <- (0 until red.dim1))
      for (j <- (0 until red.dim2)){
        var r = red.apply(i,j)
        var g = green.apply(i,j)
        var b = blue.apply(i,j)
        println(i,j,r,g,b)
        var color = new Color(r,g,b)
        resBuffImage.setRGB(j,i,color.getRGB)
      }
    resBuffImage
  }

  /*Saves the image to file for a given path and format*/
  def toFile(buffImage: BufferedImage,path: String, format: String): Unit = {
    ImageIO.write(buffImage,format,new File(path))
  }

  /*Display the image*/
  def displayImage(bufferedImage: BufferedImage): Unit = {
    val icon = new ImageIcon(bufferedImage)
    val frame = new JFrame
    frame.setLayout(new FlowLayout)
    frame.setSize(bufferedImage.getWidth, bufferedImage.getHeight)
    val lbl = new JLabel
    lbl.setIcon(icon)
    frame.add(lbl)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  }

  /*def deNoise method is for denoising the image. This is based on local neighbour mean method*/
  def deNoise(bufferedImage: BufferedImage): BufferedImage ={
    val bufimg = bufferedImage
    val rgbMatrix = getRbgMatrix(bufimg)
    val redmat = getRed(rgbMatrix)
    val greenmat = getGreen(rgbMatrix)
    val bluemat = getBlue(rgbMatrix)
    for (i <- (1 until rgbMatrix.dim1 -1))
      for(j <- (1 until rgbMatrix.dim2 -1)){
        val red = (redmat.apply(i,j) + redmat.apply(i-1,j) + redmat.apply(i+1,j) + redmat.apply(i,j-1)
          + redmat.apply(i,j+1)+redmat.apply(i-1,j-1) + redmat.apply(i+1,j+1) + redmat.apply(i-1,j+1) +redmat.apply(i+1,j-1))/9
        val green = (greenmat.apply(i,j) + greenmat.apply(i-1,j) + greenmat.apply(i+1,j)
          + greenmat.apply(i,j-1) + greenmat.apply(i,j+1) +greenmat.apply(i-1,j-1) + greenmat.apply(i+1,j+1) + greenmat.apply(i-1,j+1) +greenmat.apply(i+1,j-1))/9
        val blue = (bluemat.apply(i,j) + bluemat.apply(i-1,j) + bluemat.apply(i+1,j) + bluemat.apply(i,j-1) + bluemat.apply(i,j+1)
          +bluemat.apply(i-1,j-1) + bluemat.apply(i+1,j+1) + bluemat.apply(i-1,j+1) +bluemat.apply(i+1,j-1))/9
        bufimg.setRGB(j,i,new Color(red,green,blue).getRGB)
      }
    bufimg
  }

  /*def laplaceTransform(bufferedImage: BufferedImage): BufferedImage = {
    val bufimg = bufferedImage
    val rgbMatrix = getRbgMatrix(bufimg)
    val redmat = getRed(rgbMatrix)
    val greenmat = getGreen(rgbMatrix)
    val bluemat = getBlue(rgbMatrix)
    var lredmat = new MatrixI(rgbMatrix.dim1,rgbMatrix.dim2)
    var lbluemat = new MatrixI(rgbMatrix.dim1,rgbMatrix.dim2)
    var lgreenmat = new MatrixI(rgbMatrix.dim1,rgbMatrix.dim2)
    for (i <- (1 until rgbMatrix.dim1 -1))
      for(j <- (1 until rgbMatrix.dim2 -1)){
        val red = (redmat.apply(i,j) + redmat.apply(i-1,j) + redmat.apply(i+1,j) + redmat.apply(i,j-1)
          + redmat.apply(i,j+1))/5
        lredmat.update(i,j,5*(red - redmat.apply(i,j)))
        val green = (greenmat.apply(i,j) + greenmat.apply(i-1,j) + greenmat.apply(i+1,j)
          + greenmat.apply(i,j-1) + greenmat.apply(i,j+1))/5
        lgreenmat.update(i,j,5*(green - greenmat.apply(i,j)))
        val blue = (bluemat.apply(i,j) + bluemat.apply(i-1,j) + bluemat.apply(i+1,j)
          + bluemat.apply(i,j-1) + bluemat.apply(i,j+1))/5
        lbluemat.update(i,j,5*(blue - bluemat.apply(i,j)))
      }
    toBuffImage(subAbs(redmat,lredmat),subAbs(greenmat,lgreenmat),subAbs(bluemat,lbluemat))
  }*/
//  TODO: Needs to be developed further.
  def laplaceFilter(bufferedImage: BufferedImage): BufferedImage = {
    val bufimg = bufferedImage
    val rgbMatrix = getRbgMatrix(bufimg)
    var lmat = new MatrixI(rgbMatrix.dim1,rgbMatrix.dim2)
    for (i <- (1 until rgbMatrix.dim1 -1))
      for(j <- (1 until rgbMatrix.dim2 -1)) {
        val ns5 = (rgbMatrix.apply(i, j) + rgbMatrix.apply(i - 1, j) + rgbMatrix.apply(i + 1, j) + rgbMatrix.apply(i, j - 1)
          + rgbMatrix.apply(i, j + 1)
          +rgbMatrix.apply(i-1,j-1) + rgbMatrix.apply(i+1,j+1) + rgbMatrix.apply(i-1,j+1) +rgbMatrix.apply(i+1,j-1)) / 9
        lmat.update(i, j, 9 * (ns5 - rgbMatrix.apply(i, j)))
      }
    toBuffImage(rgbMatrix - lmat)
  }

  def subAbs(mat1: MatrixI,mat2: MatrixI): MatrixI = {
    val res = new MatrixI(mat1.dim1,mat1.dim2)
    for(i <- 0 until mat1.dim1; j<- 0 until mat2.dim2) res.update(i,j,abs(mat1.apply(i,j)-mat2.apply(i,j)))
    res
  }

  /*def smoothingMean method is one way of De-Noising the image.
  * This method takes the mean of the kernal image values and assigns it one pixel*/
  def smoothingMean(bufferedImage: BufferedImage): BufferedImage={
    val bufimg = bufferedImage
    val rgbMatrix = getRbgMatrix(bufimg)
    val redmat = getRed(rgbMatrix)
    val greenmat = getGreen(rgbMatrix)
    val bluemat = getBlue(rgbMatrix)
    for (i <- (0 until bufimg.getHeight()-2))
      for (j <- (0 until bufimg.getWidth() -2)){
        var redsmooth =0
        var gsmooth =0
        var bsmooth =0
        for(k <- (i until i+3)) {
          for (l <- (j until j + 3)) {
            redsmooth = redsmooth + redmat.apply(k, l) / 9
            gsmooth = gsmooth + greenmat.apply(k, l) / 9
            bsmooth = bsmooth + bluemat.apply(k, l) / 9
            // println(redsmooth, gsmooth, bsmooth)
          }
        }
        bufimg.setRGB(j,i,new Color(redsmooth,gsmooth,bsmooth).getRGB)
      }
    bufimg
  }

  /*def smoothingMedian method performs De-Noising by enumarates the pixel
  value based on the median of the kernal for that pixel*/
  def smoothingMedian(bufferedImage: BufferedImage,ksize: Int): BufferedImage={
    val bufimg = bufferedImage
    val rgbMatrix = getRbgMatrix(bufimg)
    val redmat = getRed(rgbMatrix)
    val greenmat = getGreen(rgbMatrix)
    val bluemat = getBlue(rgbMatrix)
    for (i <- (0 until bufimg.getHeight()-ksize-1).par)
      for (j <- (0 until bufimg.getWidth() -ksize-1).par){
        var redsmooth = new VectorI(ksize*ksize)
        var gsmooth = new VectorI(ksize*ksize)
        var bsmooth = new VectorI(ksize*ksize)
        var m=0
        for(k <- (i until i+ksize)) {
          for (l <- (j until j + ksize)) {
            redsmooth.update(m,redmat.apply(k, l))
            gsmooth.update(m,greenmat.apply(k, l))
            bsmooth.update(m,bluemat.apply(k, l))
            m = m+1
          }
        }
        redsmooth.sort()
        gsmooth.sort()
        bsmooth.sort()
        bufimg.setRGB(j,i,new Color(redsmooth.apply(redsmooth.dim/2),gsmooth.apply(gsmooth.dim/2),
          bsmooth.apply(bsmooth.dim/2)).getRGB)
      }
    bufimg
  }

  /*def toBW method converts a color image to a black and white image.
  * This method uses the iterative threshold mechanism to obtain a b/w matrix.*/
  //  TODO: Needs to be developed further.
  def toBW(rgbMatrix: MatrixI):BufferedImage={
    var bufimg = new BufferedImage(rgbMatrix.dim2,rgbMatrix.dim1,BufferedImage.TYPE_BYTE_BINARY)
    println("Red layer")
    var red = iterTreshold(getRed(rgbMatrix))
    println("Green layer")
    var green = iterTreshold(getGreen(rgbMatrix))
    println("Blue layer")
    var blue = iterTreshold(getBlue(rgbMatrix))
    for(i <- 0 until red.dim1)
      for(j <- 0 until red.dim2){
        bufimg.setRGB(j,i,new Color(red.apply(i,j),green.apply(i,j),blue.apply(i,j)).getRGB)
      }
    bufimg
  }

  /*def iterThreshold method is used as a part of converting the image into b/w.
  * This method chooses a intital threshold value as average of the max and min
  * valeus of the matrix and a new threshold value is calculated for each iteration.
  * This process stops when the difference between the old and new threshold values
  * is less than 5*/
   private def iterTreshold(matrix: MatrixI): MatrixI ={
    val max = matrix.max()
    val min = matrix.min()
    var t1 = (max+min)/2
    var t2 = 0

    while (abs(t1-t2) > 1){
      println("abs", abs(t1-t2))
      if(t2 == 0){
        t2=t1
      }
      t1 = t2
      var a1 = matrix.clean(t2.toDouble,relative = false)
      var a2 = matrix.-(a1)
      var a1avg = a1.sum/(countNonZeros(a1))
      var a2avg = a2.sum/(countNonZeros(a2))
      t2 = (a1avg + a2avg)/2
      println("t1 t2",t1,t2)
    }

    var res = matrix
    for(i <- (0 until matrix.dim1))
      for(j <- (0 until matrix.dim2)){
        if(matrix.apply(i,j)>t2){
          res.update(i,j,255)
        }
        else {
          res.update(i,j,0)
        }
      }
    res
  }

  /*def countNonZeros method returns the number of elements in the matrix that are
  * not zeros*/
  def countNonZeros(mat: MatrixI): Int = {
    var count = 0
    for (i <- 0 until mat.dim1){
      count = count + mat.apply(i).countZero
    }
    var dims = mat.dim1*mat.dim2
    abs(dims - count+1)
  }

  /*def padding method with MatrixI as argument pads 0 to the given matrix with given
  extra padD1 rows and padD2 columns*/
  def padding(mat: MatrixI,padD1: Int, padD2: Int): MatrixI ={
    var padMat = new MatrixI(mat.dim1+padD1,mat.dim2+padD2)
    padMat + mat
  }

  /*def padding with string as argument returns a BufferedImage object with x and y dims
  padded to it with 0*/
  def padding(path: String,x: Int, y: Int): BufferedImage ={
    var bufimg = getBuffImage(path)
    var mat = getRbgMatrix(bufimg)
    var padMat = new MatrixI(mat.dim1+x,mat.dim2+y)
    toBuffImage(padMat + mat)
  }

  /*def padding takes a BufferedImage and pads zeros to it for the given extra dims and
  returns the resultant BufferedImage*/
  def padding(bufferedImage: BufferedImage,x: Int, y: Int): BufferedImage ={
    var mat = getRbgMatrix(bufferedImage)
    var padMat = new MatrixI(mat.dim1+x,mat.dim2+y)
    toBuffImage(padMat + mat)
  }

}

object sampleImageIO extends App{
  //Noisy image path: '/home/vamsi/Downloads/noisy_voc_worst_002.png' || "/home/vamsi/Downloads/balloons_noisy.png"
  var bufferedImage = imageObject.getBuffImage("/home/vamsi/Downloads/balloons_noisy.png")
  var rgbMatrix = imageObject.getRbgMatrix(bufferedImage)
  var resBuffImg = imageObject.toBuffImage(rgbMatrix)
//  imageObject.displayImage(resBuffImg)
  var denoising = imageObject.smoothingMedian(resBuffImg,5)
  //imageObject.displayImage(denoising)
  var sharpen = imageObject.laplaceFilter(resBuffImg)
  imageObject.displayImage(sharpen)
}

