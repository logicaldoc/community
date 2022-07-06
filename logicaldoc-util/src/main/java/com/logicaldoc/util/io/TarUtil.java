package com.logicaldoc.util.io;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.compress.archivers.ArchiveInputStream;
import org.apache.commons.compress.archivers.ArchiveStreamFactory;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.io.IOUtils;

/**
 * This class is for reading tar files
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 8.7.1
 */
public class TarUtil {

	private TarUtil() {
	}

	public static List<String> listEntries(File tarFile) throws IOException {
		List<String> entries = new ArrayList<String>();

		try {
			try (FileInputStream fis = new FileInputStream(tarFile);
					BufferedInputStream bis = new BufferedInputStream(fis);) {
				ArchiveInputStream input = new ArchiveStreamFactory().createArchiveInputStream(bis);
				if (input instanceof TarArchiveInputStream) {
					TarArchiveInputStream tarInput = (TarArchiveInputStream) input;
					TarArchiveEntry entry = tarInput.getNextTarEntry();
					while (entry != null) {
						String name = entry.getName();
						if (name.endsWith("/"))
							name = name.substring(0, name.lastIndexOf('/'));
						entries.add(name);
						entry = tarInput.getNextTarEntry();
					}
				}
			}
		} catch (Throwable r) {
			throw new IOException(r.getMessage(), r);
		}

		return entries;
	}

	public static void extractEntry(File tarFile, String entryName, File dest) throws IOException {
		try {
			try (FileInputStream fis = new FileInputStream(tarFile);
					BufferedInputStream bis = new BufferedInputStream(fis);) {
				ArchiveInputStream input = new ArchiveStreamFactory().createArchiveInputStream(bis);
				if (input instanceof TarArchiveInputStream) {
					TarArchiveInputStream tarInput = (TarArchiveInputStream) input;
					TarArchiveEntry entry = tarInput.getNextTarEntry();
					while (entry != null) {
						if (entry.getName().equals(entryName)) {
							byte[] content = new byte[(int) entry.getSize()];
							int offset = 0;
							tarInput.read(content, offset, content.length - offset);

							try (FileOutputStream outputFile = new FileOutputStream(dest);) {
								IOUtils.write(content, outputFile);
							}

							break;
						}
						entry = tarInput.getNextTarEntry();
					}
				}
			}
		} catch (Throwable r) {
			throw new IOException(r.getMessage(), r);
		}
	}
}