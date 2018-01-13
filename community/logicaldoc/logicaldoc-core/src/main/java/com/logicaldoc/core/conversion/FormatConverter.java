package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import com.logicaldoc.core.document.Document;

/**
 * Implementations of this interface are specialized classes that perform
 * conversion from a source format to a target format.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.1.3
 */
public interface FormatConverter {

	/**
	 * Performs the conversion
	 * 
	 * @param src The source file
	 * @param dest The converted file, the extension of it's filename defines
	 *        the output format
	 * @throws IOException
	 */
	public void convert(File src, File dest) throws IOException;

	/**
	 * Performs the conversion
	 * 
	 * @param sid The actual Session ID (optional)
	 * @param document The document (optional)
	 * @param src The source file
	 * @param dest The converted file, the extension of it's filename defines
	 *        the output format
	 * @throws IOException
	 */
	public void convert(String sid, Document document, File src, File dest) throws IOException;

	/**
	 * Implementations should return the list of the required parameters. A
	 * parameter is stored in the context as converter.SimpleClassName.parameter
	 * = value
	 */
	public List<String> getParameterNames();

	/**
	 * Returns the map of parameters
	 */
	public Map<String, String> getParameters();

	/**
	 * Gets the value of a parameter
	 */
	public String getParameter(String name);

	/**
	 * Reads it's own parameters and stores them in the parameters map
	 */
	public void loadParameters();

	/**
	 * Checks if the converter is enabled or not
	 */
	public boolean isEnabled();

	/**
	 * Enables or disables the converter
	 */
	public void setEnabled(boolean enabled);
}