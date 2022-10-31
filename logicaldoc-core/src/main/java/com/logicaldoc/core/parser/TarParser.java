package com.logicaldoc.core.parser;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Locale;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.TarUtil;

/**
 * Class for parsing tar files.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public class TarParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(TarParser.class);

	@Override
	public void internalParse(InputStream input, String filename, String encoding, Locale locale, String tenant,
			Document document, String fileVersion, StringBuilder content) throws Exception {
		File tarFile = FileUtil.createTempFile("parsetar", ".tar");
		try {
			FileUtil.writeFile(input, tarFile.getAbsolutePath());
			List<String> entries = new TarUtil().listEntries(tarFile);
			if (entries.size() > 1) {
				/*
				 * If we have more entries just print the entries list
				 */
				for (String line : entries) {
					content.append(line);
					content.append("\n");
				}
			} else {
				/*
				 * If we have just one entry, parse it
				 */
				String entry = entries.get(0);
				String entryExtension = FileUtil.getExtension(entry);
				File uncompressedEntryFile = FileUtil.createTempFile("parse", "." + entryExtension);
				try {
					Parser entryParser = ParserFactory.getParser(entryExtension);
					if (entryParser == null)
						throw new IOException(String.format("Unable to find a parser for %s", entryExtension));

					new TarUtil().extractEntry(tarFile, entry, uncompressedEntryFile);

					Document clone = new Document(document);
					clone.setFileName(uncompressedEntryFile.getName());
					String text = entryParser.parse(uncompressedEntryFile, uncompressedEntryFile.getName(), encoding,
							locale, tenant, document, fileVersion);
					content.append(text);
				} finally {
					if (uncompressedEntryFile != null)
						FileUtil.strongDelete(uncompressedEntryFile);
				}
			}
		} finally {
			if (tarFile != null)
				FileUtil.strongDelete(tarFile);
		}
	}

	/**
	 * The number of pages is the number entries
	 */
	@Override
	public int countPages(InputStream input, String filename) {
		File tarFile = null;
		try {
			tarFile = FileUtil.createTempFile("parsetar", ".tar");
			FileUtil.writeFile(input, tarFile.getAbsolutePath());
			return countPages(tarFile, filename);
		} catch (Throwable t) {
			log.error(t.getMessage(), t);
		} finally {
			if (tarFile != null)
				FileUtil.strongDelete(tarFile);
		}
		return 1;
	}

	@Override
	public int countPages(File input, String filename) {
		try {
			List<String> entries = new TarUtil().listEntries(input);
			if (entries.size() > 1) {
				return entries.size();
			} else {
				/*
				 * If we have just one entry, count it's pages
				 */
				String entry = entries.get(0);
				String entryExtension = FileUtil.getExtension(entry);
				File uncompressedEntryFile = FileUtil.createTempFile("parse", "." + entryExtension);
				try {
					Parser entryParser = ParserFactory.getParser(entryExtension);
					if (entryParser == null)
						throw new IOException(String.format("Unable to find a parser for %s", entryExtension));

					new TarUtil().extractEntry(input, entry, uncompressedEntryFile);
					return entryParser.countPages(uncompressedEntryFile, uncompressedEntryFile.getName());
				} finally {
					if (uncompressedEntryFile != null)
						FileUtil.strongDelete(uncompressedEntryFile);
				}
			}
		} catch (Throwable e) {
			log.error(e.getMessage(), e);
		}
		return 1;
	}
}