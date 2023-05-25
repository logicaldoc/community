package com.logicaldoc.util.io;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import com.github.junrar.Archive;
import com.github.junrar.exception.RarException;
import com.github.junrar.rarfile.FileHeader;
import com.logicaldoc.util.Context;

/**
 * This class is for reading RAR files
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 8.7.1
 */
public class RarUtil {

	/**
	 * Maximum size of the uncompressed contents of the compressed archive,
	 * config parameter zip.maxsize
	 */
	private int maxSize = 1024 * 1024 * 1024; // 1 GB

	public RarUtil() {
		try {
			maxSize = Context.get().getProperties().getInt("zip.maxsize", 1024) * 1024 * 1024;
		} catch (Exception t) {
			// Nothing to do
		}
	}

	public List<String> listEntries(File rarFile) throws IOException {
		List<String> entries = new ArrayList<>();

		try {
			try (Archive archive = new Archive(rarFile)) {
				FileHeader fh = archive.nextFileHeader();
				while (fh != null) {
					entries.add(fh.getFileName());
					fh = archive.nextFileHeader();
				}
			}
		} catch (RarException e) {
			throw new IOException(e.getMessage());
		}

		return entries;
	}

	public void extractEntry(File rarFile, String entryName, File dest) throws IOException {
		try {
			try (Archive archive = new Archive(rarFile)) {
				FileHeader fh = archive.nextFileHeader();
				while (fh != null) {
					if (entryName.equals(fh.getFileName())) {
						try (BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(dest));) {
							int nBytes = -1;
							byte[] buffer = new byte[4096];
							int totalSizeEntry = 0;

							try (InputStream is = archive.getInputStream(fh);) {
								while ((nBytes = is.read(buffer)) > 0) {
									bos.write(buffer, 0, nBytes);
									totalSizeEntry += nBytes;

									if (totalSizeEntry > maxSize)
										throw new IOException(String.format(
												"Rar file looks like a Zip Bomb Attack: the uncompressed data size is over the maximum allowed of %s",
												FileUtil.getDisplaySize(maxSize, "en")));
								}
							}
							bos.flush();
						}
						break;
					}
					fh = archive.nextFileHeader();
				}
			}
		} catch (RarException e) {
			throw new IOException(e.getMessage());
		}
	}
}