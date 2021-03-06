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


class Image(path: String) {

  /*def getBuffImage() returns the buffered image object on the path.*/
  private def getBuffImage(): java.awt.image.BufferedImage = {
    val image = new File(this.path)
    ImageIO.read(image)
  }

  var buffImage: BufferedImage = getBuffImage()
  var rbgMatrix : MatrixI = getRbgMatrix()
  /*def getRgbArray method returns a array of RGB values of size image.width * image.height*/
  def getRbgMatrix(): MatrixI = {
    val image = this.buffImage
    var pixels = new Array[Int](image.getWidth*image.getHeight)
    toMatrix(image.getRGB(0,0,image.getWidth,image.getHeight,pixels,0,image.getWidth),image.getHeight,image.getWidth)
  }

  /*def getRed method returns vectorI with values red channel from the image*/
  def getRed(): scalation.linalgebra.MatrixI = {
    val rgb = getRbgMatrix().apply()
    var red = rgb.map(arr => arr.map(x => new Color(x).getRed))
    new MatrixI(red)
  }

  /*def getGreen method returns vectorI with values green channel from the image*/
  def getGreen(): scalation.linalgebra.MatrixI = {
    val rgb = this.getRbgMatrix().apply()
    var green = rgb.map(arr => arr.map(x => new Color(x).getGreen))
    new MatrixI(green)
  }

  /*def getBlue method returns vectorI with values blue channel from the image*/
  def getBlue(): scalation.linalgebra.MatrixI = {
    val rgb = this.getRbgMatrix().apply()
    var blue = rgb.map(arr => arr.map(x => new Color(x).getBlue))
    new MatrixI(blue)
  }

  /*def getAlpha method returns vectorI with values alpha channel from the image*/
  def getAlpha(): scalation.linalgebra.MatrixI = {
    val rgb = this.getRbgMatrix().apply()
    var alpha = rgb.map(arr => arr.map(x => new Color(x).getAlpha))
    new MatrixI(alpha)
  }

  /*Returns height of the image*/
  def height(): Int = {
    this.buffImage.getHeight
  }

  /*Returns width of the image*/
  def width(): Int = {
    this.buffImage.getWidth
  }

  /*Converts the image into a grey scale image and return MatrixI object*/
  def toGrey(): Unit = {
    var red = getRed().*=(21)./(100)
    var blue = getBlue().*=(8)./(100)
    var green = getGreen().*=(71)./(100)
    val bwarr = red.+(green).+(blue)
    this.buffImage = toBuffImage(bwarr)
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
        var color = new Color(rbg,rbg,rbg)
        resBuffImage.setRGB(j,i,color.getRGB)
      }
    resBuffImage
  }

  /*Converts a 'this' rbgMatrix matrixI for  to buffered image*/
  def toBuffImage(): BufferedImage = {
    var mat = rbgMatrix
    val width = rbgMatrix.dim2
    val height = rbgMatrix.dim1
    var resBuffImage = new BufferedImage(width,height,BufferedImage.TYPE_INT_RGB)
    for (i <- (0 until height))
      for (j <- (0 until width)){
        var rbg = mat.apply(i,j)
        var color = new Color(rbg,rbg,rbg)
        resBuffImage.setRGB(j,i,color.getRGB)
      }
    resBuffImage
  }

  /*Saves the image to file for a given path and format*/
  def toFile(path: String, format: String): Unit = {
    ImageIO.write(this.buffImage,format,new File(path))
  }

  /*Display the images*/
  def displayImage(): Unit = {
    val icon = new ImageIcon(this.buffImage)
    val frame = new JFrame
    frame.setLayout(new FlowLayout)
    frame.setSize(this.buffImage.getWidth, this.buffImage.getHeight)
    val lbl = new JLabel
    lbl.setIcon(icon)
    frame.add(lbl)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  }

  /*def deNoise method is for denoising the image. This is based on local neighbour mean method*/
  private def deNoise(): Unit ={
    val bufimg = buffImage
    val redmat = getRed()
    val greenmat = getGreen()
    val bluemat = getBlue()
    for (i <- (1 until this.height()-1))
      for(j <- (1 until this.width()-1)){
        val red = (redmat.apply(i,j) + redmat.apply(i-1,j) + redmat.apply(i+1,j) + redmat.apply(i,j-1)
          + redmat.apply(i,j+1)+redmat.apply(i-1,j-1) + redmat.apply(i+1,j+1) + redmat.apply(i-1,j+1) +redmat.apply(i+1,j-1))/9
        val green = (greenmat.apply(i,j) + greenmat.apply(i-1,j) + greenmat.apply(i+1,j)
          + greenmat.apply(i,j-1) + greenmat.apply(i,j+1) +greenmat.apply(i-1,j-1) + greenmat.apply(i+1,j+1) + greenmat.apply(i-1,j+1) +greenmat.apply(i+1,j-1))/9
        val blue = (bluemat.apply(i,j) + bluemat.apply(i-1,j) + bluemat.apply(i+1,j) + bluemat.apply(i,j-1) + bluemat.apply(i,j+1)
          +bluemat.apply(i-1,j-1) + bluemat.apply(i+1,j+1) + bluemat.apply(i-1,j+1) +bluemat.apply(i+1,j-1))/9
        bufimg.setRGB(j,i,new Color(red,green,blue).getRGB)
      }
    buffImage = bufimg
  }

  /*def smoothingMean method is one way of De-Noising the image.
  * This method takes the mean of the kernal image values and assigns it one pixel*/
  def smoothingMean(): Unit={
    val bufimg = buffImage
    val redmat = getRed()
    val greenmat = getGreen()
    val bluemat = getBlue()
    for (i <- (0 until this.height()-2))
      for (j <- (0 until this.width() -2)){
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
    buffImage = bufimg
  }

  /*def smoothingMedian method performs De-Noising by enumarates the pixel
  value based on the median of the kernal for that pixel*/
  def smoothingMedian(ksize: Int): Unit={
    val bufimg = buffImage
    val redmat = getRed()
    val greenmat = getGreen()
    val bluemat = getBlue()
    for (i <- (0 until this.height()-ksize-1))
      for (j <- (0 until this.width() -ksize-1)){
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
    buffImage = bufimg
  }

  /*def toBW method converts a color image to a black and white image.
  * This method uses the iterative threshold mechanism to obtain a b/w matrix.*/
  def toBW():Unit={
    var bufimg = new BufferedImage(this.width(),this.height(),BufferedImage.TYPE_BYTE_BINARY)
    println("Red layer")
    var red = iterTreshold(getRed())
    println("Green layer")
    var green = iterTreshold(getGreen())
    println("Blue layer")
    var blue = iterTreshold(getBlue())
    for(i <- (0 until red.dim1))
      for(j <- (0 until red.dim2)){
        bufimg.setRGB(j,i,new Color(red.apply(i,j),green.apply(i,j),blue.apply(i,j)).getRGB)
      }
    buffImage = bufimg
  }

  /*def iterThreshold method is used as a part of converting the image into b/w.
  * This method chooses a intital threshold value as average of the max and min
  * valeus of the matrix and a new threshold value is calculated for each iteration.
  * This process stops when the difference between the old and new threshold values
  * is less than 5*/
  def iterTreshold(matrix: MatrixI): MatrixI ={
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
    for (i <- (0 until mat.dim1)){
      count = count + mat.apply(i).countZero
    }
    var dims = mat.dim1*mat.dim2
    abs(dims - count+1)
  }

  /*deNoising method calls deNoise() method repeatedly to reduce the noise of the image */
  def deNoising(): Unit ={
    deNoise()
    deNoise()
    deNoise()
    deNoise()
  }

}

object imageIO extends App{
  //Noisy image path: '/home/vamsi/Downloads/noisy_voc_worst_002.png' || "/home/vamsi/Downloads/balloons_noisy.png"
  val img = new Image("/home/vamsi/Desktop/sample.jpg")
  val noisyimg = new Image("/home/vamsi/Downloads/kodim23-noise-std51.png")
  val noisyimg2 = new Image("/home/vamsi/Downloads/balloons_noisy.png")
  noisyimg2.getRed()
  noisyimg2.smoothingMedian(5)
  noisyimg2.displayImage()
}

