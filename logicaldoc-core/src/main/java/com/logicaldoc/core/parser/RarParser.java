package com.logicaldoc.core.parser;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.Locale;

import org.apache.commons.io.FilenameUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.RarUtil;

/**
 * Class for parsing rar files.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.7.1
 */
public class RarParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(RarParser.class);

	@Override
	public void internalParse(InputStream input, String filename, String encoding, Locale locale, String tenant,
			Document document, String fileVersion, StringBuffer content) throws Exception {
		File rarFile = File.createTempFile("parserar", ".rar");
		try {
			FileUtil.writeFile(input, rarFile.getAbsolutePath());
			List<String> entries = RarUtil.listEntries(rarFile);
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
				File uncompressedEntryFile = File.createTempFile("parse", "." + entryExtension);
				try {
					Parser entryParser = ParserFactory.getParser(entryExtension);
					if (entryParser == null)
						throw new IOException(String.format("Unable to find a parser for %s", entryExtension));

					RarUtil.extractEntry(rarFile, entry, uncompressedEntryFile);

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
			if (rarFile != null)
				FileUtil.strongDelete(rarFile);
		}
	}

	/**
	 * The number of pages is the number entries
	 */
	@Override
	public int countPages(InputStream input, String filename) {
		File rarFile = null;
		try {
			rarFile = File.createTempFile("parserar", ".rar");
			FileUtil.writeFile(input, rarFile.getAbsolutePath());
			return countPages(rarFile, filename);
		} catch (Throwable t) {
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
			List<String> entries = RarUtil.listEntries(input);
			if (entries.size() > 1) {
				return entries.size();
			} else {
				/*
				 * If we have just one entry, count it's pages
				 */
				String entry = entries.get(0);
				String entryExtension = FileUtil.getExtension(entry);
				File uncompressedEntryFile = File.createTempFile("parse", "." + entryExtension);
				try {
					Parser entryParser = ParserFactory.getParser(entryExtension);
					if (entryParser == null)
						throw new IOException(String.format("Unable to find a parser for %s", entryExtension));

					RarUtil.extractEntry(input, entry, uncompressedEntryFile);
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