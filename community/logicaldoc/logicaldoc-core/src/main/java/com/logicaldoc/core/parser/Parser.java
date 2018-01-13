package com.logicaldoc.core.parser;

import java.io.File;
import java.io.InputStream;
import java.util.Locale;

/**
 * @author Michael Scholz
 */
public interface Parser {

	public String getVersion();

	public String getContent();

	public String getAuthor();

	public String getSourceDate();

	public String getTags();

	public String getTitle();

	/**
	 * The original file name of the content to be parsed
	 */
	public String getFilename();

	public void setFilename(String filename);

	/**
	 * The locale of the content to be parsed
	 */
	public Locale getLocale();

	public void setLocale(Locale locale);

	/**
	 * The character encoding of the content to be parsed
	 */
	public String getEncoding();

	public void setEncoding(String encoding);
	
	/**
	 * The tenant name
	 */
	public String getTenant();

	public void setTenant(String tenant);

	/**
	 * Same as the other method that accept an input stream, use this when you
	 * have a file rather than a stream.
	 * 
	 * @param file
	 */
	public void parse(File file);

	/**
	 * Extracts content for the text content of the given binary document. The
	 * content type and character encoding (if available and applicable) are
	 * given as arguments. The given content type is guaranteed to be one of the
	 * types reported by {@link #getContentTypes()} unless the implementation
	 * explicitly permits other content types.
	 * <p>
	 * The implementation can choose either to read and parse the given document
	 * immediately or to return a reader that does it incrementally. The only
	 * constraint is that the implementation must close the given stream latest
	 * when the returned reader is closed. The caller on the other hand is
	 * responsible for closing the returned reader.
	 * <p>
	 * The implementation should only throw an exception on transient errors,
	 * i.e. when it can expect to be able to successfully extract the text
	 * content of the same binary at another time. An effort should be made to
	 * recover from syntax errors and other similar problems.
	 * <p>
	 * This method should be thread-safe, i.e. it is possible that this method
	 * is invoked simultaneously by different threads to extract the text
	 * content of different documents. On the other hand the returned reader
	 * does not need to be thread-safe.
	 * <p>
	 * The parsing has to be completed before the seconds specified in the
	 * <b>parser.timeout</b> config. property.
	 * 
	 * @param input binary document from which to extract text
	 */
	public void parse(InputStream input);
}