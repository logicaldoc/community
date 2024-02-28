package com.logicaldoc.core.parser;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ZipUtil;

/**
 * Class for parsing zip files.
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.5
 */
public class ZipParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(ZipParser.class);

	@Override
	public void internalParse(InputStream input, ParseParameters parameters, StringBuilder content)
			throws IOException, ParseException {

		if (parameters.getFileName().toLowerCase().endsWith(".zip"))
			internalParseZip(input, parameters, content);
		else
			internalParseGZip(input, parameters, content);
	}

	private void internalParseGZip(InputStream input, ParseParameters parameters, StringBuilder content)
			throws IOException, ParseException {
		File ungzippedFile = null;
		try {
			ungzippedFile = gunzip(input, parameters.getFileName());
			Parser parser = ParserFactory.getParser(ungzippedFile.getName());

			if (parser != null) {
				String cnt = parser.parse(ungzippedFile, ungzippedFile.getName(), parameters.getEncoding(),
						parameters.getLocale(), parameters.getTenant(), parameters.getDocument(),
						parameters.getFileVersion());
				content.append(cnt);
			}
		} finally {
			if (ungzippedFile != null)
				FileUtil.strongDelete(ungzippedFile);
		}
	}

	private File gunzip(File input, String fileName) throws IOException {
		try (InputStream is = new FileInputStream(input)) {
			return gunzip(is, fileName);
		}
	}

	private File gunzip(InputStream input, String fileName) throws IOException {
		String unpackedFileName = fileName.toLowerCase().endsWith(".tgz") ? FileUtil.getBaseName(fileName) + ".tar.gz"
				: fileName;
		unpackedFileName = unpackedFileName.substring(0, unpackedFileName.lastIndexOf('.'));
		File ungzippedFile = FileUtil.createTempFile("parsegzip",
				"." + FileUtil.getExtension(unpackedFileName).toLowerCase());
		ZipUtil zipUtil = new ZipUtil();
		zipUtil.unGZip(input, ungzippedFile);
		return ungzippedFile;
	}

	private void internalParseZip(InputStream input, ParseParameters parameters, StringBuilder content)
			throws IOException, ParseException {
		File zipFile = FileUtil.createTempFile("parsezip", "zip");
		try {
			FileUtil.writeFile(input, zipFile.getAbsolutePath());
			ZipUtil zipUtil = new ZipUtil();
			List<String> entries = zipUtil.listEntries(zipFile);

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

					zipUtil.unzip(zipFile, entry, uncompressedEntryFile);

					Document clone = new Document(parameters.getDocument());
					clone.setFileName(uncompressedEntryFile.getName());
					String text = entryParser.parse(uncompressedEntryFile, uncompressedEntryFile.getName(),
							parameters.getEncoding(), parameters.getLocale(), parameters.getTenant(),
							parameters.getDocument(), parameters.getFileVersion());
					content.append(text);
				} finally {
					if (uncompressedEntryFile != null)
						FileUtil.strongDelete(uncompressedEntryFile);
				}
			}
		} finally {
			FileUtil.strongDelete(zipFile);
		}
	}

	@Override
	public int countPages(InputStream input, String filename) {
		if (filename.toLowerCase().endsWith(".zip")) {
			File zipFile = null;
			try {
				zipFile = FileUtil.createTempFile("parsezip", "zip");
				FileUtil.writeFile(input, zipFile.getAbsolutePath());
				return countPages(zipFile, filename);
			} catch (Exception t) {
				log.error(t.getMessage(), t);
			} finally {
				if (zipFile != null)
					FileUtil.strongDelete(zipFile);
			}
		} else {
			File ungzippedFile = null;
			try {
				ungzippedFile = gunzip(input, filename);
				Parser parser = ParserFactory.getParser(ungzippedFile.getName());
				if (parser != null)
					return parser.countPages(ungzippedFile, ungzippedFile.getName());
			} catch (Exception t) {
				log.error(t.getMessage(), t);
			} finally {
				if (ungzippedFile != null)
					FileUtil.strongDelete(ungzippedFile);
			}
		}
		return 1;
	}

	/**
	 * The number of pages is the number entries
	 */
	@Override
	public int countPages(File input, String filename) {
		if (filename.toLowerCase().endsWith(".zip")) {
			try {
				return countPagesInZipFile(input);
			} catch (Exception e) {
				log.error(e.getMessage(), e);
			}
		} else {
			File ungzippedFile = null;
			try {
				ungzippedFile = gunzip(input, filename);
				Parser parser = ParserFactory.getParser(ungzippedFile.getName());
				if (parser != null)
					return parser.countPages(ungzippedFile, ungzippedFile.getName());
			} catch (Exception t) {
				log.error(t.getMessage(), t);
			} finally {
				if (ungzippedFile != null)
					FileUtil.strongDelete(ungzippedFile);
			}
		}
		return 1;
	}

	private int countPagesInZipFile(File zipFile) throws IOException {
		ZipUtil zipUtil = new ZipUtil();
		List<String> entries = zipUtil.listEntries(zipFile);
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

				zipUtil.unzip(zipFile, entry, uncompressedEntryFile);
				return entryParser.countPages(uncompressedEntryFile, uncompressedEntryFile.getName());
			} finally {
				if (uncompressedEntryFile != null)
					FileUtil.strongDelete(uncompressedEntryFile);
			}
		}
	}
}