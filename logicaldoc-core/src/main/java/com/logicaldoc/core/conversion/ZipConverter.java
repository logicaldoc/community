
package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;
import java.util.List;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ZipUtil;
import com.logicaldoc.util.spring.Context;

/**
 * Converts a Zip file in it's listing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.1
 */
public class ZipConverter extends CompressedArchiveConverter {

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
			FormatConverterManager manager = Context.get(FormatConverterManager.class);
			FormatConverter converter = manager.getConverter(ungzippedFile.getName(), dest.getName());
			if (converter != null)
				converter.convert(sid, document, ungzippedFile, dest);
		} finally {
			if (ungzippedFile != null)
				FileUtil.delete(ungzippedFile);
		}
	}

	private File gunzip(File input, String fileName) throws IOException {
		String unpackedFileName = fileName.toLowerCase().endsWith(".tgz") ? FileUtil.getBaseName(fileName) + ".tar.gz"
				: fileName;
		unpackedFileName = unpackedFileName.substring(0, unpackedFileName.lastIndexOf('.'));
		File ungzippedFile = FileUtil.createTempFile("parsegzip",
				"." + FileUtil.getExtension(unpackedFileName).toLowerCase());
		try (ZipUtil zipUtil = new ZipUtil();) {
			zipUtil.unGZip(input, ungzippedFile);
			return ungzippedFile;
		}
	}

	private void convertZip(String sid, Document document, File src, File dest) throws IOException {
		try (ZipUtil zipUtil = new ZipUtil();) {
			List<String> entries = zipUtil.listEntries(src);
			if (entries.size() > 1)
				convertMultipleEntries(sid, document, dest, entries);
			else
				convertSingleEntry(sid, document, src, dest, entries.get(0));
		}
	}

	@Override
	protected void extractEntry(File archiveFile, String entry, File uncompressedEntryFile) throws IOException {
		try (ZipUtil zipUtil = new ZipUtil();) {
			zipUtil.unzip(archiveFile, entry, uncompressedEntryFile);
		}
	}
}