package com.logicaldoc.util.io;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.compress.archivers.ArchiveException;
import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.ArchiveStreamFactory;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;

import com.logicaldoc.util.Context;

/**
 * This class is for reading tar files
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 8.7.1
 */
public class TarUtil {

	/**
	 * Maximum size of the uncompressed contents of the compressed archive,
	 * config parameter zip.maxsize
	 */
	private int maxSize = 1024 * 1024 * 1024; // 1 GB

	public TarUtil() {
		try {
			maxSize = Context.get().getProperties().getInt("zip.maxsize", 1024) * 1024 * 1024;
		} catch (Throwable t) {
			// Nothing to do
		}
	}

	public List<String> listEntries(File tarFile) throws IOException {
		List<String> entries = new ArrayList<String>();

		try {
			try (FileInputStream fis = new FileInputStream(tarFile);
					BufferedInputStream bis = new BufferedInputStream(fis);
					ArchiveInputStream input = new ArchiveStreamFactory().createArchiveInputStream(bis);) {

				if (input instanceof TarArchiveInputStream) {
					TarArchiveInputStream tarInput = null;
					try {
						tarInput = (TarArchiveInputStream) input;
						TarArchiveEntry entry = tarInput.getNextTarEntry();
						while (entry != null) {
							String name = entry.getName();
							if (name.endsWith("/"))
								name = name.substring(0, name.lastIndexOf('/'));
							entries.add(name);
							entry = tarInput.getNextTarEntry();
						}
					} finally {
						if (tarInput != null)
							tarInput.close();
					}
				}
			}
		} catch (ArchiveException e) {
			throw new IOException(e.getMessage());
		}

		return entries;
	}

	public void extractEntry(File tarFile, String entryName, File dest) throws IOException {
		try {
			try (FileInputStream fis = new FileInputStream(tarFile);
					BufferedInputStream bis = new BufferedInputStream(fis);
					ArchiveInputStream input = new ArchiveStreamFactory().createArchiveInputStream(bis);
					BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(dest));) {

				if (input instanceof TarArchiveInputStream) {
					try (TarArchiveInputStream tarInput = (TarArchiveInputStream) input;) {
						int nBytes = -1;
						byte[] buffer = new byte[4096];
						int totalSizeEntry = 0;

						while ((nBytes = tarInput.read(buffer)) > 0) {
							bos.write(buffer, 0, nBytes);
							totalSizeEntry += nBytes;

							if (totalSizeEntry > maxSize)
								throw new IOException(String.format(
										"Tar file looks like a Zip Bomb Attack: the uncompressed data size is over the maximum allowed of %s",
										FileUtil.getDisplaySize(maxSize, "en")));
						}

						bos.flush();
					}
				}
			}
		} catch (ArchiveException e) {
			throw new IOException(e.getMessage());
		}
	}
}