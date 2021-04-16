package com.logicaldoc.util;

import java.io.File;
import java.io.IOException;

import org.apache.commons.io.FileUtils;

import com.logicaldoc.util.io.ZipUtil;

public class UtilWorkbench {

	/**
	 * @param args
	 * @throws IOException
	 */
	public static void main(String[] args) throws IOException {
//		String command="C:\\LogicalDOC-Devel\\imagemagick\\convert.exe -compress JPEG -quality 90 -resize x200 C:\\tmp\\test.jpg C:\\tmp\\tile.jpg";
//		Exec.exec(command, null, null, 10);
		
//		new ZipUtil().unGZipUnTar(new File("C:\\Users\\marco\\Downloads\\GeoLite2-City_20201201.tar.gz"),
//				new File("target/ungzip"));
//		System.out.println("Unzipped");
//		FileUtils.forceDelete(new File("target/ungzip"));
//		System.out.println("Deleted");
		
		System.out.println(StringUtil.printFileSize(0));
	}
}