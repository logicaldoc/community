package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ZipUtil;

/**
 * Converts a Zip file in it's listing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.1
 */
public class ZipConverter extends AbstractFormatConverter {

	protected static Logger log = LoggerFactory.getLogger(ZipConverter.class);

	@Override
	public void internalConvert(String sid, Document document, File src, File dest) throws IOException {
		if ((document != null && document.getFileName() != null
				&& document.getFileName().toLowerCase().endsWith(".zip"))
				|| (src != null && src.getName().toLowerCase().endsWith(".zip")))
			convertZip(sid, document, src, dest);
		else
			convertGZip(sid, document, src, dest);
	}

	private void convertGZip(String sid, Document document, File src, File dest) throws IOException {
		if (src == null)
			throw new IOException("No source");

		File ungzippedFile = null;
		try {
			ungzippedFile = gunzip(src,
					(document != null && document.getFileName() != null) ? document.getFileName() : src.getName());
			FormatConverterManager manager = (FormatConverterManager) Context.get()
					.getBean(FormatConverterManager.class);
			FormatConverter converter = manager.getConverter(ungzippedFile.getName(), dest.getName());
			if (converter != null)
				converter.convert(sid, document, ungzippedFile, dest);
		} finally {
			if (ungzippedFile != null)
				FileUtil.strongDelete(ungzippedFile);
		}
	}

	private File gunzip(File input, String fileName) throws IOException {
		String unpackedFileName = fileName.toLowerCase().endsWith(".tgz")
				? FilenameUtils.getBaseName(fileName) + ".tar.gz"
				: fileName;
		unpackedFileName = unpackedFileName.substring(0, unpackedFileName.lastIndexOf('.'));
		File ungzippedFile = File.createTempFile("parsegzip",
				"." + FileUtil.getExtension(unpackedFileName).toLowerCase());
		ZipUtil zipUtil = new ZipUtil();
		zipUtil.unGZip(input, ungzippedFile);
		return ungzippedFile;
	}

	private void convertZip(String sid, Document document, File src, File dest) throws IOException {
		ZipUtil zipUtil = new ZipUtil();
		List<String> entries = zipUtil.listEntries(src);
		if (entries.size() > 1)
			unzipMultipleEntries(sid, document, src, dest, entries);
		else
			unzipSingleEntry(sid, document, src, dest, entries.get(0));

	}

	private void unzipSingleEntry(String sid, Document document, File src, File dest, String entry) throws IOException {
		String entryExtension = FileUtil.getExtension(entry);
		File uncompressedEntryFile = File.createTempFile("unzip", "." + entryExtension);

		String targetExtension = FileUtil.getExtension(dest.getName()).toLowerCase();

		try {
			ZipUtil zipUtil = new ZipUtil();
			zipUtil.unzipEntry(src, entry, uncompressedEntryFile);
			FormatConverterManager manager = (FormatConverterManager) Context.get()
					.getBean(FormatConverterManager.class);
			FormatConverter converter = manager.getConverter(entryExtension, targetExtension);

			if (converter == null)
				throw new IOException(
						String.format("Unable to find a converter from %s to %s", entryExtension, targetExtension));
			Document clone = new Document(document);
			clone.setFileName(uncompressedEntryFile.getName());
			converter.convert(sid, document, uncompressedEntryFile, dest);
		} finally {
			if (uncompressedEntryFile != null)
				FileUtil.strongDelete(uncompressedEntryFile);
		}
	}

	private void unzipMultipleEntries(String sid, Document document, File src, File dest, List<String> entries)
			throws IOException {
		File tempFile = File.createTempFile("zipconvert", ".txt");
		try (FileWriter writer = new FileWriter(tempFile);) {
			for (String line : entries) {
				writer.write(line);
				writer.write("\n");
			}
			writer.flush();

			String targetExtension = FileUtil.getExtension(dest.getName()).toLowerCase();
			if ("txt".equals(targetExtension)) {
				FileUtil.copyFile(tempFile, dest);
			} else if ("pdf".equals(targetExtension)) {
				FormatConverterManager manager = (FormatConverterManager) Context.get()
						.getBean(FormatConverterManager.class);
				FormatConverter converter = manager.getConverter("txt", targetExtension);

				if (converter == null)
					throw new IOException(
							String.format("Unable to find a converter from %s to %s", "txt", targetExtension));
				converter.convert(sid, document, tempFile, dest);
			}

			if (!dest.exists() || dest.length() < 1)
				throw new Exception("Empty conversion");
		} catch (Throwable e) {
			throw new IOException("Error in Zip conversion", e);
		} finally {
			if (tempFile != null)
				FileUtil.strongDelete(tempFile);
		}
	}
}