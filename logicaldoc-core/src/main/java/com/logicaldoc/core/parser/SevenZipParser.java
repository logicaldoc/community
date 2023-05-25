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
import com.logicaldoc.util.io.SevenZipUtil;

/**
 * Class for parsing 7z files.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public class SevenZipParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(SevenZipParser.class);

	@Override
	public void internalParse(InputStream input, String filename, String encoding, Locale locale, String tenant,
			Document document, String fileVersion, StringBuilder content) throws IOException, ParseException {
		File sevenFile = FileUtil.createTempFile("parse7z", ".7z");
		try {
			FileUtil.writeFile(input, sevenFile.getAbsolutePath());
			List<String> entries = new SevenZipUtil().listEntries(sevenFile);
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

					new SevenZipUtil().extractEntry(sevenFile, entry, uncompressedEntryFile);

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
			if (sevenFile != null)
				FileUtil.strongDelete(sevenFile);
		}
	}

	/**
	 * The number of pages is the number entries
	 */
	@Override
	public int countPages(InputStream input, String filename) {
		File rarFile = null;
		try {
			rarFile = FileUtil.createTempFile("parse7z", ".7z");
			FileUtil.writeFile(input, rarFile.getAbsolutePath());
			return countPages(rarFile, filename);
		} catch (Exception t) {
			log.error(t.getMessage(), t);
		} finally {
			if (rarFile != null)
				FileUtil.strongDelete(rarFile);
		}
		return 1;
	}

	/**
	 * The number of pages is the number entries
	 */
	@Override
	public int countPages(File input, String filename) {
		try {
			List<String> entries = new SevenZipUtil().listEntries(input);
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

					new SevenZipUtil().extractEntry(input, entry, uncompressedEntryFile);
					return entryParser.countPages(uncompressedEntryFile, uncompressedEntryFile.getName());
				} finally {
					if (uncompressedEntryFile != null)
						FileUtil.strongDelete(uncompressedEntryFile);
				}
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
		return 1;
	}
}