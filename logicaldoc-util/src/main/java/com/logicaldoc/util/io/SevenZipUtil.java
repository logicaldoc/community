package com.logicaldoc.util.io;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.compress.archivers.sevenz.SevenZArchiveEntry;
import org.apache.commons.compress.archivers.sevenz.SevenZFile;

import com.logicaldoc.util.spring.Context;

/**
 * This class is for handling with 7z files
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 8.7.1
 */
public class SevenZipUtil {

	/**
	 * Maximum size of the uncompressed contents of the compressed archive,
	 * config parameter zip.maxsize
	 */
	private int maxSize = 1024 * 1024 * 1024; // 1 GB

	public SevenZipUtil() {
		try {
			maxSize = Context.get().getConfig().getInt("zip.maxsize", 1024) * 1024 * 1024;
		} catch (Exception t) {
			// Nothing to do
		}
	}

	public List<String> listEntries(File sevenZipFile) throws IOException {
		List<String> entries = new ArrayList<>();

		try (SevenZFile archiveFile = SevenZFile.builder().setFile(sevenZipFile).get()) {
			SevenZArchiveEntry entry;
			while ((entry = archiveFile.getNextEntry()) != null)
				entries.add(entry.getName());
		} catch (IOException e) {
			throw new IOException(e.getMessage(), e);
		}

		return entries;
	}

	public void extractEntry(File sevenZipFile, String entryName, File dest) throws IOException {
		try (SevenZFile archiveFile = SevenZFile.builder().setFile(sevenZipFile).get()) {
			SevenZArchiveEntry entry;
			while ((entry = archiveFile.getNextEntry()) != null) {
				if (entry.getName().equals(entryName) && entry.hasStream()) {
					try (FileOutputStream fileOutputStream = new FileOutputStream(dest)) {
						int length = -1;
						byte[] buffer = new byte[2048];
						int totalSizeEntry = 0;

						while ((length = archiveFile.read(buffer)) != -1) {
							fileOutputStream.write(buffer, 0, length);
							totalSizeEntry += length;

							if (totalSizeEntry > maxSize)
								throw new IOException(String.format(
										"7Zip file looks like a Zip Bomb Attack: the uncompressed data size is over the maximum allowed of %s",
										FileUtil.getDisplaySize(maxSize, "en")));
						}
					}
					break;
				}
			}
		}
	}
}