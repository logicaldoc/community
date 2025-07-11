
package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;

/**
 * A base class for those converters that operate on compressed archives
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.3
 */
public abstract class CompressedArchiveConverter extends AbstractFormatConverter {

	protected void convertMultipleEntries(String sid, Document document, File dest, List<String> entries)
			throws IOException {
		File tempFile = FileUtil.createTempFile("zipconvert", ".txt");
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
				FormatConverterManager manager = Context.get(FormatConverterManager.class);
				FormatConverter converter = manager.getConverter("txt", targetExtension);

				if (converter == null)
					throw new IOException(
							String.format("Unable to find a converter from %s to %s", "txt", targetExtension));
				converter.convert(sid, document, tempFile, dest);
			}

			if (!dest.exists() || dest.length() < 1)
				throw new IOException("Empty conversion");
		} catch (IOException ioe) {
			throw ioe;
		} catch (Exception e) {
			throw new IOException("Error in Zip conversion", e);
		} finally {
			if (tempFile != null)
				FileUtil.delete(tempFile);
		}
	}

	protected void convertSingleEntry(String sid, Document document, File src, File dest, String entry)
			throws IOException {
		String entryExtension = FileUtil.getExtension(entry);
		File uncompressedEntryFile = FileUtil.createTempFile("unzip", "." + entryExtension);

		String targetExtension = FileUtil.getExtension(dest.getName()).toLowerCase();
		try {
			extractEntry(src, entry, uncompressedEntryFile);
			FormatConverterManager manager = Context.get(FormatConverterManager.class);
			FormatConverter converter = manager.getConverter(entryExtension, targetExtension);

			if (converter == null)
				throw new IOException(
						String.format("Unable to find a converter from %s to %s", entryExtension, targetExtension));
			Document clone = new Document(document);
			clone.setFileName(uncompressedEntryFile.getName());
			converter.convert(sid, document, uncompressedEntryFile, dest);
		} finally {
			if (uncompressedEntryFile != null)
				FileUtil.delete(uncompressedEntryFile);
		}
	}

	/**
	 * Unpacks an entry in a target file
	 * 
	 * @param archiveFile the compressed archive
	 * @param entry the entry to extract
	 * @param uncompressedEntryFile the target file where the entry will be
	 *        unapacked
	 * 
	 * @throws IOException a generic I/O error
	 */
	protected abstract void extractEntry(File archiveFile, String entry, File uncompressedEntryFile) throws IOException;
}