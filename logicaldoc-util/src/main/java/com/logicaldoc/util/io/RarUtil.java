package com.logicaldoc.util.io;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import com.github.junrar.Archive;
import com.github.junrar.rarfile.FileHeader;

/**
 * This class is for reading RAR files
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 8.7.1
 */
public class RarUtil {

	public RarUtil() {
	}

	public static List<String> listEntries(File rarFile) throws IOException {
		List<String> entries = new ArrayList<String>();

		try {
			try (Archive archive = new Archive(rarFile)) {
				FileHeader fh = archive.nextFileHeader();
				while (fh != null) {
					entries.add(fh.getFileName());
					fh = archive.nextFileHeader();
				}
			}
		} catch (Throwable r) {
			throw new IOException(r.getMessage(), r);
		}

		return entries;
	}

	public static void extractEntry(File rarFile, String entryName, File dest) throws IOException {
		try {
			try (Archive archive = new Archive(rarFile)) {
				FileHeader fh = archive.nextFileHeader();
				while (fh != null) {
					if (entryName.equals(fh.getFileName())) {
						try(FileOutputStream os = new FileOutputStream(dest)){
							archive.extractFile(fh, os);
						}
						break;
					}
					fh = archive.nextFileHeader();
				}
			}
		} catch (Throwable r) {
			throw new IOException(r.getMessage(), r);
		}
	}

//	File f = new File(filename);
//    Archive archive = new Archive(f);
//    archive.getMainHeader().print();
//    FileHeader fh = archive.nextFileHeader();
//    while(fh!=null){        
//            File fileEntry = new File(fh.getFileNameString().trim());
//            System.out.println(fileEntry.getAbsolutePath());
//            FileOutputStream os = new FileOutputStream(fileEntry);
//            archive.extractFile(fh, os);
//            os.close();
//            fh=archive.nextFileHeader();
//    }
}