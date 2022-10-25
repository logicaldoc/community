package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.io.SevenZipUtil;

/**
 * Converts a 7z file in it's listing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public class SevenZipConverter extends CompressedArchiveConverter {

	protected static Logger log = LoggerFactory.getLogger(SevenZipConverter.class);

	@Override
	public void internalConvert(String sid, Document document, File src, File dest) throws IOException {
		List<String> entries = new SevenZipUtil().listEntries(src);
		if (entries.size() > 1)
			convertMultipleEntries(sid, document, src, dest, entries);
		else
			convertSingleEntry(sid, document, src, dest, entries.get(0));
	}

	@Override
	protected void extractEntry(File archiveFile, String entry, File uncompressedEntryFile) throws IOException {
		new SevenZipUtil().extractEntry(archiveFile, entry, uncompressedEntryFile);
	}
}