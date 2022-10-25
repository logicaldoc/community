package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.io.TarUtil;

/**
 * Converts a Tar file in it's listing
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public class TarConverter extends CompressedArchiveConverter {

	protected static Logger log = LoggerFactory.getLogger(TarConverter.class);

	@Override
	public void internalConvert(String sid, Document document, File src, File dest) throws IOException {
		List<String> entries = new TarUtil().listEntries(src);
		if (entries.size() > 1)
			convertMultipleEntries(sid, document, src, dest, entries);
		else
			convertSingleEntry(sid, document, src, dest, entries.get(0));
	}

	@Override
	protected void extractEntry(File archiveFile, String entry, File uncompressedEntryFile) throws IOException {
		new TarUtil().extractEntry(archiveFile, entry, uncompressedEntryFile);
	}
}