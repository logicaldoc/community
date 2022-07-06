package com.logicaldoc.util.io;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.compress.archivers.sevenz.SevenZArchiveEntry;
import org.apache.commons.compress.archivers.sevenz.SevenZFile;

/**
 * This class is for handling with 7z files
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 8.7.1
 */
public class SevenZipUtil {

	private SevenZipUtil() {
	}

	public static List<String> listEntries(File sevenZipFile) throws IOException {
		List<String> entries = new ArrayList<String>();

		try (SevenZFile archiveFile = new SevenZFile(sevenZipFile);) {
			SevenZArchiveEntry entry;
			while ((entry = archiveFile.getNextEntry()) != null)
				entries.add(entry.getName());
		} catch (Throwable r) {
			throw new IOException(r.getMessage(), r);
		}

		return entries;
	}

	public static void extractEntry(File sevenZipFile, String entryName, File dest) throws IOException {
		try (SevenZFile archiveFile = new SevenZFile(sevenZipFile);) {
			SevenZArchiveEntry entry;
			while ((entry = archiveFile.getNextEntry()) != null) {
				if (entry.getName().equals(entryName) && entry.hasStream()) {
					try (FileOutputStream fileOutputStream = new FileOutputStream(dest)) {
						int length = -1;
						byte[] buffer = new byte[2048];
						while ((length = archiveFile.read(buffer)) != -1) {
							fileOutputStream.write(buffer, 0, length);
						}
					}
					break;
				}
			}
		} catch (Throwable r) {
			throw new IOException(r.getMessage(), r);
		}
	}
}