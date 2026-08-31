package com.logicaldoc.core.parser;

import java.io.File;
import java.io.InputStream;
import java.util.Locale;

import com.logicaldoc.core.document.Document;

/**
 * A Parser is capable of parsing a content in order to extract the texts and
 * other metadata within it. Concrete implementations of this interface are
 * designed to process specific file types.
 * 
 * @author Michael Scholz
 */
public interface Parser {

	/**
	 * Same as {@link #parse(InputStream, String, String, Locale, String)}, use
	 * this when you have a file rather than a stream.
	 * 
	 * @param file the file
	 * @param filename name of the file
	 * @param encoding character encoding
	 * @param locale the locale
	 * @param tenant name of the tenant
	 * 
	 * @return the extracted text
	 * 
	 * @throws ParsingException error in the parsing
	 */
	public String parse(File file, String filename, String encoding, Locale locale, String tenant)
			throws ParsingException;

	/**
	 * Same as {@link #parse(InputStream, ParseParameters)}, but use this when
	 * you have a file rather than a stream.
	 * 
	 * @param file the file
	 * @param filename name of the file
	 * @param encoding character encoding
	 * @param locale the locale
	 * @param tenant name of the tenant
	 * @param document the document the file belongs to (optional)
	 * @param fileVersion the file version being processed (optional)
	 * 
	 * @return the extracted text
	 * 
	 * @throws ParsingException error in the parsing
	 */
	public String parse(File file, String filename, String encoding, Locale locale, String tenant, Document document,
			String fileVersion) throws ParsingException;

	/**
	 * Extracts content for the text content of the given binary document. The
	 * content type and character encoding (if available and applicable) are
	 * given as arguments.
	 * <p>
	 * The implementation can choose either to read and parse the given document
	 * immediately or to return a reader that does it incrementally. The only
	 * constraint is that the implementation must close the given stream latest
	 * when the returned reader is closed. The caller on the other hand is
	 * responsible for closing the returned reader.
	 * </p>
	 * <p>
	 * The implementation should only throw an exception on transient errors,
	 * i.e. when it can expect to be able to successfully extract the text
	 * content of the same binary at another time. An effort should be made to
	 * recover from syntax errors and other similar problems.
	 * </p>
	 * <p>
	 * This method should be thread-safe, i.e. it is possible that this method
	 * is invoked simultaneously by different threads to extract the text
	 * content of different documents. On the other hand the returned reader
	 * does not need to be thread-safe.
	 * </p>
	 * <p>
	 * The parsing has to be completed before the seconds specified in the
	 * <b>parser.timeout</b> config. property.
	 * </p>
	 * 
	 * <p>
	 * Depending on the value of the <b>parser.timeout.retain</b> config.
	 * property, the already extracted text is retained or not in case of
	 * timeout.
	 * </p>
	 * 
	 * @param input binary content from which to extract the text
	 * @param parameterObject the parameters
	 * @return the extracted text
	 * 
	 * @throws ParsingException error in the parsing
	 */
	public String parse(InputStream input, ParseParameters parameterObject) throws ParsingException;

	/**
	 * Extracts content for the text content of the given binary document. The
	 * content type and character encoding (if available and applicable) are
	 * given as arguments.
	 * <p>
	 * The implementation can choose either to read and parse the given document
	 * immediately or to return a reader that does it incrementally. The only
	 * constraint is that the implementation must close the given stream latest
	 * when the returned reader is closed. The caller on the other hand is
	 * responsible for closing the returned reader.
	 * </p>
	 * <p>
	 * The implementation should only throw an exception on transient errors,
	 * i.e. when it can expect to be able to successfully extract the text
	 * content of the same binary at another time. An effort should be made to
	 * recover from syntax errors and other similar problems.
	 * </p>
	 * <p>
	 * This method should be thread-safe, i.e. it is possible that this method
	 * is invoked simultaneously by different threads to extract the text
	 * content of different documents. On the other hand the returned reader
	 * does not need to be thread-safe.
	 * </p>
	 * <p>
	 * The parsing has to be completed before the seconds specified in the
	 * <b>parser.timeout</b> config. property.
	 * </p>
	 * 
	 * <p>
	 * Depending on the value of the <b>parser.timeout.retain</b> config.
	 * property, the already extracted text is retained or not in case of
	 * timeout.
	 * </p>
	 * 
	 * @param input binary content from which to extract the text
	 * @param filename name of the file
	 * @param encoding character encoding
	 * @param locale the locale
	 * @param tenant name of the tenant
	 * 
	 * @return the extracted text
	 * 
	 * @throws ParsingException error in the parsing
	 */
	public String parse(InputStream input, String filename, String encoding, Locale locale, String tenant)
			throws ParsingException;

	/**
	 * Counts the number of pages of the given binary document.
	 * 
	 * @param input binary content from which to extract the text
	 * @param filename name of the file
	 * 
	 * @return the number of pages
	 */
	public int countPages(InputStream input, String filename);

	/**
	 * Same as the other {@link #countPages(InputStream, String)}, but use this
	 * when you have a file rather than a stream.
	 * 
	 * @param file the file
	 * @param filename name of the file
	 * 
	 * @return the number of pages
	 */
	public int countPages(File file, String filename);
}