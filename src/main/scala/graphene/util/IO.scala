package graphene.util

import org.apache.commons.io.FileUtils
import java.io.File


object IO {
  val workdirPath = System.getenv("WORKDIR")

  val workdir = new File(workdirPath)

  def file(name: String) = new File(workdir, name)
  def save(str: String, f: File) {FileUtils.write(f, str, "UTF-8")}
}