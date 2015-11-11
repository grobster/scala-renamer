object MyDate {
	def toDoubleDigit(n: Int): String = n match {
		case 1 => "01"
		case 2 => "02"
		case 3 => "03"
		case 4 => "04"
		case 5 => "05"
		case 6 => "06"
		case 7 => "07"
		case 8 => "08"
		case 9 => "09"
		case _ => n.toString
	}
	
	def YYMMDD: String = {
		import java.time._
		val now = LocalDate.now
		val year = now.getYear
		val month = now.getMonthValue
		val day = now.getDayOfMonth
		val date = year.toString.substring(2) + toDoubleDigit(month) + toDoubleDigit(day)
		date	
	}
}

import scala.annotation.tailrec
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardCopyOption.ATOMIC_MOVE
import java.nio.file.StandardCopyOption.REPLACE_EXISTING
import java.nio.file.Files
import java.io.File

class Renamer(val basePath: Path) {
	val JPEG = ".jpg"
	
	def newFileName(newBasePath: Path): Renamer = new Renamer(newBasePath)
	
	def autoFolderPath: Path = { Paths.get(basePath.toString + System.getProperty("file.separator") + MyDate.YYMMDD) }
	
	def manualFolderPath(s: String): Path = { Paths.get(basePath.toString + System.getProperty("file.separator") + s) }
	
	def otherPath(s: String): Path = { Paths.get(s) }
	
	def isValidGid(s: String): Boolean = { s.length == 8 && s(0) == 'Z' && s(1) == '0' }
	
	def findGid(ls: List[String]): Option[String] = {
		@tailrec
		def _find(ls: List[String]): Option[String] = ls match {
			case Nil => None
			case x :: ys if(isValidGid(x)) => Some(x)
			case x :: ys if(!isValidGid(x)) => _find(ys)
		}
		_find(ls)
	}
	
	def rename(file: Path, completePath: Path, ending: String): Path = {
		val gidList = file.getFileName.toString.split("\\.").toList
		val gid = findGid(gidList).getOrElse(gidList(0) + " " + gidList(1))
		val target = Paths.get(completePath.toString + System.getProperty("file.separator") + gid + ending)
		Files.move(file, target, REPLACE_EXISTING, ATOMIC_MOVE)
	}
	
	def renameFiles(completePath: Path, ending: String): Unit = {
		val files = completePath.toFile.listFiles
		for(file <- files) { rename(file.toPath, completePath, ending)}
	}
	
	def renameFiles2(completePath: Path, ending: String): Unit = {
		val li = completePath.toFile.listFiles.toList.map { (f: File) => f.toPath }
		li.par.map(rename(_, completePath, ending))
	}
}