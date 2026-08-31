package com.logicaldoc.core.parser;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

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

	private static final Logger log = LoggerFactory.getLogger(RarParser.class);

	@Override
	public void internalParse(InputStream input, ParseParameters parameters, StringBuilder content)
			throws ParsingException, IOException {
		File rarFile = FileUtil.createTempFile("parserar", ".rar");
		try {
			FileUtil.writeFile(input, rarFile.getAbsolutePath());
			List<String> entries = new RarUtil().listEntries(rarFile);
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
						throw new ParsingException(String.format("Unable to find a parser for %s", entryExtension));

					new RarUtil().extractEntry(rarFile, entry, uncompressedEntryFile);

					Document clone = new Document(parameters.getDocument());
					clone.setFileName(uncompressedEntryFile.getName());
					String text = entryParser.parse(uncompressedEntryFile, uncompressedEntryFile.getName(),
							parameters.getEncoding(), parameters.getLocale(), parameters.getTenant(),
							parameters.getDocument(), parameters.getFileVersion());
					content.append(text);
				} finally {
					if (uncompressedEntryFile != null)
						FileUtil.delete(uncompressedEntryFile);
				}
			}
		} finally {
			FileUtil.delete(rarFile);
		}
	}

	/**
	 * The number of pages is the number entries
	 */
	@Override
	public int countPages(InputStream input, String filename) {
		File rarFile = null;
		try {
			rarFile = FileUtil.createTempFile("parserar", ".rar");
			FileUtil.writeFile(input, rarFile.getAbsolutePath());
			return countPages(rarFile, filename);
		} catch (Exception t) {
			log.error(t.getMessage(), t);
		} finally {
			if (rarFile != null)
				FileUtil.delete(rarFile);
		}
		return 1;
	}

	/**
	 * The number of pages is the number entries
	 */
	@Override
	public int countPages(File input, String filename) {
		try {
			List<String> entries = new RarUtil().listEntries(input);
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

					new RarUtil().extractEntry(input, entry, uncompressedEntryFile);
					return entryParser.countPages(uncompressedEntryFile, uncompressedEntryFile.getName());
				} finally {
					if (uncompressedEntryFile != null)
						FileUtil.delete(uncompressedEntryFile);
				}
			}
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}
		return 1;
	}
}