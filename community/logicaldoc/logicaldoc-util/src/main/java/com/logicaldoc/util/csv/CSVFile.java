package com.logicaldoc.util.csv;

/**
 * CSVFile is a class used to handle <a href="http://en.wikipedia.org/wiki/Comma-separated_values">Comma-Separated Values</a> files.
 * <p>
 * It is abstract because it is the base class used for {@link CSVFileReader} and {@link CSVFileWriter}
 * so you should use one of these (or both) according on what you need to do.
 * <p>
 * The simplest example for using the classes contained in this package is {@link CSVFileExample}, that simply
 * converts one CSV file into another one that makes use of a different notation for field separator
 * and text qualifier.<br>
 * The example just comprises the following lines:
 * <p>
 * <pre>
 * import java.util.*;
 * import java.io.*;
 *
 * public class CSVFileExample {
 *
 * 	public static void main(String[] args) throws FileNotFoundException,IOException {
 *
 * 		CSVFileReader in = new CSVFileReader("csv_in.txt", ';', '"');
 * 		CSVFileWriter out = new CSVFileWriter("csv_out.txt", ',', '\'');
 *
 *     Vector<String> fields = in.readFields();
 *     while(fields!=null) {
 *       out.writeFields(fields);
 *       fields = in.readFields();
 *     }
 *
 *     in.close();
 *     out.close();
 *  }
 *
 * }
 * </pre>
 *
 * @author  Fabrizio Fazzino
 * @version %I%, %G%
 */
public abstract class CSVFile {

	/**
	 * The default char used as field separator.
	 */
  protected static final char DEFAULT_FIELD_SEPARATOR = ',';

	/**
	 * The default char used as text qualifier
	 */
  protected static final char DEFAULT_TEXT_QUALIFIER = '"';

	/**
	 * The current char used as field separator.
	 */
  protected char fieldSeparator;

	/**
	 * The current char used as text qualifier.
	 */
  protected char textQualifier;

	/**
	 * CSVFile constructor with the default field separator and text qualifier.
	 */
  public CSVFile() {
    this(DEFAULT_FIELD_SEPARATOR, DEFAULT_TEXT_QUALIFIER);
  }

	/**
	 * CSVFile constructor with a given field separator and the default text qualifier.
	 *
	 * @param sep The field separator to be used; overwrites the default one
	 */
  public CSVFile(char sep) {
    this(sep, DEFAULT_TEXT_QUALIFIER);
  }

	/**
	 * CSVFile constructor with given field separator and text qualifier.
	 *
	 * @param sep  The field separator to be used; overwrites the default one
	 * @param qual The text qualifier to be used; overwrites the default one
	 */
  public CSVFile(char sep, char qual) {
    setFieldSeparator(sep);
    setTextQualifier(qual);
  }

	/**
	 * Set the current field separator.
	 *
	 * @param sep The new field separator to be used; overwrites the old one
	 */
  public void setFieldSeparator(char sep) {
    fieldSeparator = sep;
  }

	/**
	 * Set the current text qualifier.
	 *
	 * @param qual The new text qualifier to be used; overwrites the old one
	 */
  public void setTextQualifier(char qual) {
    textQualifier = qual;
  }

	/**
	 * Get the current field separator.
	 *
	 * @return The char containing the current field separator
	 */
  public char getFieldSeparator() {
    return fieldSeparator;
  }

	/**
	 * Get the current text qualifier.
	 *
	 * @return The char containing the current text qualifier
	 */
  public char getTextQualifier() {
    return textQualifier;
  }

}

