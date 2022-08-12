package com.logicaldoc.util.io;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.RandomAccessFile;
import java.io.Writer;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.security.MessageDigest;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.StringTokenizer;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.tools.ant.types.selectors.SelectorUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.SystemUtil;

/**
 * This class manages I/O operations with files.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 4.0
 */
public class FileUtil {
	static final int BUFF_SIZE = 8192;

	static final byte[] buffer = new byte[BUFF_SIZE];

	protected static Logger log = LoggerFactory.getLogger(FileUtil.class);

	/**
	 * Writes a stream to a file and then closes the same stream.
	 * 
	 * @param in the input stream
	 * @param filepath the target file path
	 * 
	 * @throws Exception raised in case of I/O error
	 */
	public static void writeFile(InputStream in, String filepath) throws Exception {
		OutputStream os = null;
		try {
			os = new FileOutputStream(filepath);

			while (true) {
				synchronized (buffer) {
					int amountRead = in.read(buffer);
					if (amountRead == -1)
						break;
					os.write(buffer, 0, amountRead);
				}
			}
		} finally {
			if (os != null)
				os.flush();
			try {
				if (in != null)
					in.close();
				if (os != null)
					os.close();
			} catch (IOException e) {
				logError(e.getMessage());
			}
		}
	}

	public static void writeFile(byte[] in, String filepath) throws Exception {
		InputStream inStream = null;
		try {
			inStream = new ByteArrayInputStream(in);
			writeFile(inStream, filepath);
		} finally {
			try {
				if (inStream != null)
					inStream.close();
			} catch (IOException e) {
				logError(e.getMessage());
			}
		}
	}

	public static void writeFile(String text, String filepath) {
		BufferedOutputStream bos = null;

		try {
			bos = new BufferedOutputStream(new FileOutputStream(filepath));
			bos.write(text.getBytes("UTF-8"));
		} catch (Throwable e) {
			logError(e.getLocalizedMessage());
		} finally {
			if (bos != null) {
				try {
					bos.flush();
					bos.close();
				} catch (Throwable ioe) {
				}
			}
		}
	}

	public static String readFile(File file) throws IOException {
		FileInputStream fisTargetFile = null;
		try {
			fisTargetFile = new FileInputStream(file);
			return IOUtils.toString(fisTargetFile, "UTF-8");
		} finally {
			try {
				fisTargetFile.close();
			} catch (Throwable ioe) {
			}
		}
	}

	public static String readFile(String filePath) throws IOException {
		return readFile(new File(filePath));
	}

	public static void appendFile(String text, String filepath) {
		OutputStream bos = null;

		try {
			bos = new FileOutputStream(filepath, true);
			bos.write(text.getBytes());
		} catch (Exception e) {
			logError(e.getLocalizedMessage());
		} finally {
			if (bos != null) {
				try {
					bos.close();
				} catch (IOException ioe) {
					;
				}
			}
		}
	}

	private static void logError(String message) {
		Log logger = LogFactory.getLog(FileUtil.class);
		logger.error(message);
	}

	public static String computeDigest(InputStream is) {
//		try {
//			return  DigestUtils.sha1Hex(is);
//		} catch (IOException e) {
//			return null;
//		}

		String digest = "";
		MessageDigest sha = null;

		try {
			if (is != null) {
				sha = MessageDigest.getInstance("SHA-1");
				byte[] message = new byte[BUFF_SIZE];
				int len = 0;
				while ((len = is.read(message)) != -1) {
					sha.update(message, 0, len);
				}
				byte[] messageDigest = sha.digest();
				// convert the array to String
				int size = messageDigest.length;
				StringBuffer buf = new StringBuffer();
				int unsignedValue = 0;
				String strUnsignedValue = null;
				for (int i = 0; i < size; i++) {
					// convert each messageDigest byte to unsigned
					unsignedValue = ((int) messageDigest[i]) & 0xff;
					strUnsignedValue = Integer.toHexString(unsignedValue);
					// at least two letters
					if (strUnsignedValue.length() == 1)
						buf.append("0");
					buf.append(strUnsignedValue);
				}
				digest = buf.toString();
				log.debug("Computed Digest: {}", digest);

				return digest;
			}
		} catch (IOException io) {
			log.error("Error generating digest: ", io);
		} catch (Throwable t) {
			log.error("Error generating digest: ", t);
		} finally {
			try {
				is.close();
			} catch (IOException e) {
			}
		}
		return null;
	}

	/**
	 * This method calculates the digest of a file using the algorithm SHA-1.
	 * 
	 * @param file The file for which will be computed the digest
	 * @return digest
	 */
	public static String computeDigest(File file) {
		InputStream is;
		try {
			is = new BufferedInputStream(new FileInputStream(file), BUFF_SIZE);
			return computeDigest(is);
		} catch (FileNotFoundException e) {
			log.error(e.getMessage());
		}
		return null;
	}

	/**
	 * This method calculates the digest of a string using the algorithm SHA-1.
	 * 
	 * @param src The string for which will be computed the digest
	 * 
	 * @return digest
	 */
	public static String computeDigest(String src) {
		InputStream is;
		try {
			is = IOUtils.toInputStream(src, "UTF-8");
			return computeDigest(is);
		} catch (Throwable e) {
			log.error(e.getMessage());
		}
		return null;
	}

	/**
	 * This method calculates the digest of a file using the algorithm SHA-1.
	 * 
	 * @param file The file for which will be computed the digest
	 * @return digest
	 */
	public static byte[] computeSha1Hash(File file) {
		InputStream is = null;
		try {
			is = new BufferedInputStream(new FileInputStream(file), BUFF_SIZE);
			return computeSha1Hash(is);
		} catch (IOException io) {
			log.error(io.getMessage(), io);
		} finally {
			try {
				is.close();
			} catch (IOException e) {
			}
		}
		return null;
	}

	/**
	 * This method calculates the digest of a inputStram content using the
	 * algorithm SHA-1.
	 * 
	 * @param is The content of which will be computed the digest
	 * @return digest
	 */
	public static byte[] computeSha1Hash(InputStream is) {
		MessageDigest sha = null;
		try {
			if (is != null) {
				sha = MessageDigest.getInstance("SHA-1");
				byte[] message = new byte[BUFF_SIZE];
				int len = 0;
				while ((len = is.read(message)) != -1) {
					sha.update(message, 0, len);
				}
				byte[] messageDigest = sha.digest();
				return messageDigest;
			}
		} catch (IOException io) {
			log.error("Error generating SHA-1: ", io);
		} catch (Throwable t) {
			log.error("Error generating SHA-1: ", t);
		}
		return null;
	}

	/**
	 * Writes the specified classpath resource into a file
	 * 
	 * @param resourceName Fully qualified resource name
	 * @param out The output file
	 * 
	 * @throws IOException if the copy caused an error
	 */
	public static void copyResource(String resourceName, File out) throws IOException {
		InputStream is = null;
		OutputStream os = null;
		try {
			try {
				is = new BufferedInputStream(FileUtil.class.getResource(resourceName).openStream());
			} catch (Exception e) {
				is = new BufferedInputStream(
						Thread.currentThread().getContextClassLoader().getResource(resourceName).openStream());
			}
			os = new BufferedOutputStream(new FileOutputStream(out));

			for (;;) {
				int b = is.read();
				if (b == -1)
					break;
				os.write(b);
			}
		} finally {
			if (is != null)
				is.close();
			if (os != null)
				os.close();
		}
	}

	/**
	 * Computes the folder size as the sum of all files directly and indirectly
	 * contained.
	 * 
	 * @param folder the folder to calculate
	 * 
	 * @return the sum of the sizes of all contained files expressed in bytes
	 */
	public static long getFolderSize(File folder) {
		long foldersize = 0;

		File[] files = folder.listFiles();
		for (int i = 0; i < files.length; i++) {
			if (files[i].isDirectory()) {
				foldersize += getFolderSize(files[i]);
			} else {
				foldersize += files[i].length();
			}
		}
		return foldersize;
	}

	/**
	 * Renders a file size in a more readable behaviour taking into account the
	 * user locale. Depending on the size, the result will be presented in the
	 * following measure units: GB, MB, KB or Bytes
	 * 
	 * @param size Size to be rendered
	 * @param language The language for the format symbols
	 * 
	 * @return the size as human readable text
	 */
	public static String getDisplaySize(long size, String language) {
		String displaySize = "";
		Locale locale = new Locale("en");
		if (StringUtils.isNotEmpty(language))
			locale = new Locale(language);
		NumberFormat nf = new DecimalFormat("###,###,###.0", new DecimalFormatSymbols(locale));
		if (size > 1000000000) {
			displaySize = nf.format((double) size / 1024 / 1024 / 1024) + " GB";
		} else if (size > 1000000) {
			displaySize = nf.format((double) size / 1024 / 1024) + " MB";
		} else if (size > 1000) {
			displaySize = nf.format((double) size / 1024) + " KB";
		} else {
			displaySize = size + " Bytes";
		}
		return displaySize;
	}

	/**
	 * Renders a file size in a more readable behaviour taking into account the
	 * user locale. The size is always rendered in the KB(kilobyte) measure
	 * unit.
	 * 
	 * @param size Size to be rendered
	 * @param language The language for the format symbols
	 * 
	 * @return the size in KB as human readable text
	 */
	public static String getDisplaySizeKB(long size, String language) {
		String displaySize = "";
		Locale locale = new Locale("en");
		if (StringUtils.isNotEmpty(language))
			locale = new Locale(language);
		NumberFormat nf = new DecimalFormat("###,###,##0.0", new DecimalFormatSymbols(locale));
		displaySize = nf.format((double) size / 1024) + " KB";
		return displaySize;
	}

	/**
	 * Check if a given filename matches the <code>includes</code> and not the
	 * <code>excludes</code>
	 * 
	 * @param filename The filename to consider
	 * @param includes list of includes expressions (eg. *.doc,*dummy*)
	 * @param excludes list of excludes expressions (eg. *.doc,*dummy*)
	 * 
	 * @return true only if the passed filename matches the includes and not the
	 *         excludes
	 */
	public static boolean matches(String filename, String[] includes, String[] excludes) {
		// First of all check if the filename must be excluded
		if (excludes != null && excludes.length > 0)
			for (String s : excludes)
				if (StringUtils.isNotEmpty(s) && SelectorUtils.match(s, filename, false))
					return false;

		// Then check if the filename must can be included
		if (includes != null && includes.length > 0)
			for (String s : includes)
				if (StringUtils.isNotEmpty(s) && SelectorUtils.match(s, filename, false))
					return true;

		if (includes == null || includes.length == 0)
			return true;
		else
			return false;
	}

	/**
	 * Check if a given filename matches the <code>includes</code> and not the
	 * <code>excludes</code>
	 * 
	 * @param filename The filename to consider
	 * @param includes comma-separated list of includes expressions (eg.
	 *        *.doc,*dummy*)
	 * @param excludes comma-separated list of excludes expressions (eg.
	 *        *.doc,*dummy*)
	 * @return true only if the passed filename matches the includes and not the
	 *         excludes
	 */
	public static boolean matches(String filename, String includes, String excludes) {
		List<String> inc = new ArrayList<String>();
		List<String> exc = new ArrayList<String>();

		StringTokenizer st;

		if (StringUtils.isNotEmpty(excludes)) {
			st = new StringTokenizer(excludes, ",", false);
			while (st.hasMoreTokens())
				exc.add(st.nextToken().trim());
		}

		if (StringUtils.isNotEmpty(includes)) {
			st = new StringTokenizer(includes, ",", false);
			while (st.hasMoreTokens())
				inc.add(st.nextToken().trim());
		}

		return matches(filename, inc.toArray(new String[0]), exc.toArray(new String[0]));
	}

	public static void writeUTF8(String content, File file, boolean append) {
		BufferedWriter out = null;
		try {
			out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file, append), "UTF8"));
			out.write(content);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			if (out != null)
				try {
					out.close();
				} catch (IOException e) {

				}
		}
	}

	public static byte[] toByteArray(File file) {
		InputStream is = null;
		try {
			is = new BufferedInputStream(new FileInputStream(file), 2048);
			return IOUtils.toByteArray(is);
		} catch (IOException e) {
			log.error(e.getMessage());
		} finally {
			if (is != null)
				try {
					is.close();
				} catch (IOException e) {
				}
		}
		return null;
	}

	public static byte[] toByteArray(RandomAccessFile input, long start, long length) throws IOException {
		ByteArrayOutputStream output = null;
		try {
			// Open streams.
			output = new ByteArrayOutputStream();
			copy(input, output, start, length);
			output.flush();
			return output.toByteArray();
		} finally {
			try {
				input.close();
			} catch (Throwable e) {
			}
			try {
				output.close();
			} catch (Throwable e) {
			}
		}
	}

	public static byte[] toByteArray(File file, long start, long length) throws IOException {
		return toByteArray(new RandomAccessFile(file, "r"), start, length);
	}

	/**
	 * Copies the input file into the output at the given offset
	 * 
	 * @param input the file to copy
	 * @param output the target file to copy to
	 * @param offset an offset to use in copying from the input expressed in
	 *        number of bytes
	 * 
	 * @throws IOException raised in case of error
	 */
	public static void copy(File input, File output, long offset) throws IOException {
		RandomAccessFile inputRa = new RandomAccessFile(input, "r");
		RandomAccessFile outputRa = new RandomAccessFile(output, "rw");
		FileChannel sourceChannel = inputRa.getChannel();
		FileChannel targetChannel = outputRa.getChannel();
		try {
			targetChannel.transferFrom(sourceChannel, offset, input.length());
		} finally {
			try {
				sourceChannel.close();
			} catch (Throwable e) {

			}

			try {
				inputRa.close();
			} catch (Throwable e) {

			}

			try {
				targetChannel.close();
			} catch (Throwable e) {

			}

			try {
				outputRa.close();
			} catch (Throwable e) {

			}
		}
	}

	/**
	 * Copy the given byte range of the given input to the given output.
	 * 
	 * @param input The input to copy the given range to the given output for.
	 * @param output The output to copy the given range from the given input
	 *        for.
	 * @param start Start of the byte range.
	 * @param length Length of the byte range.
	 * @throws IOException If something fails at I/O level.
	 */
	private static void copy(RandomAccessFile input, OutputStream output, long start, long length) throws IOException {
		byte[] buffer = new byte[BUFF_SIZE];
		int read;

		if (input.length() == length) {
			// Write full range.
			while ((read = input.read(buffer)) > 0) {
				output.write(buffer, 0, read);
			}
		} else {
			// Write partial range.
			input.seek(start);
			long toRead = length;

			while ((read = input.read(buffer)) > 0) {
				if ((toRead -= read) > 0) {
					output.write(buffer, 0, read);
				} else {
					output.write(buffer, 0, (int) toRead + read);
					break;
				}
			}
		}
	}

	public static void replaceInFile(String sourcePath, String token, String newValue) throws Exception {
		BufferedReader reader = null;
		Writer writer = null;
		String oldContent = "";

		File tmp = new File(sourcePath + ".tmp");
		File file = new File(sourcePath);

		try {
			reader = new BufferedReader(new InputStreamReader(new FileInputStream(sourcePath), "UTF-8"));

			// Reading all the lines of input text file into oldContent
			String line = reader.readLine();

			while (line != null) {
				oldContent = oldContent + line + System.lineSeparator();
				line = reader.readLine();
			}

			// Replacing oldString with newString in the oldContent
			String newContent = oldContent.replaceAll(token, newValue);

			// Rewriting the input text file with newContent
			writer = new OutputStreamWriter(new FileOutputStream(tmp), "UTF-8");

			writer.write(newContent);
		} catch (IOException ioe) {
			log.error(ioe.getMessage(), ioe);
		} finally {
			try {
				// Closing the resources
				reader.close();
				writer.close();
			} catch (IOException e) {
				log.error(e.getMessage(), e);
			}
		}

		file.delete();
		tmp.renameTo(file);
	}

	/**
	 * Quickest way to copy a file in Java, makes use of NIO buffer.
	 * 
	 * @param source the source file to copy
	 * @param target the target file
	 * 
	 * @throws IOException if the copy resulted in an error
	 */
	@SuppressWarnings("resource")
	public static void copyFile(File source, File target) throws IOException {
		FileChannel in = null;
		FileChannel out = null;

		try {
			in = new FileInputStream(source).getChannel();
			out = new FileOutputStream(target).getChannel();

			ByteBuffer buffer = ByteBuffer.allocateDirect(BUFF_SIZE);
			while (in.read(buffer) != -1) {
				// For Java8 compatibility
				((Buffer) buffer).flip();

				while (buffer.hasRemaining()) {
					out.write(buffer);
				}
				// For Java8 compatibility
				((Buffer) buffer).clear();
			}
		} catch (IOException e) {
			log.warn(e.getMessage());
		} finally {
			close(in);
			close(out);
		}
	}

	private static void close(Closeable closable) {
		if (closable != null) {
			try {
				closable.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	public static void strongDelete(File file) {
		if (file == null || !file.exists())
			return;

		try {
			if (file != null && file.exists())
				FileUtils.forceDelete(file);
		} catch (IOException e) {
			log.warn(e.getMessage());
		}

		if (file != null && file.exists())
			try {
				log.debug("Delete file {} using OS command", file.getAbsolutePath());
				if (SystemUtil.isUnix() || SystemUtil.isMac() || SystemUtil.isSolaris())
					java.lang.Runtime.getRuntime().exec("rm -rf \"" + file.getAbsolutePath() + "\"");
				else
					java.lang.Runtime.getRuntime().exec("cmd /C del /F /Q \"" + file.getAbsolutePath() + "\"");
			} catch (IOException e) {
				log.warn(e.getMessage(), e);
			}
	}

	public static boolean isDirEmpty(final Path directory) throws IOException {
		try (DirectoryStream<Path> dirStream = Files.newDirectoryStream(directory)) {
			return !dirStream.iterator().hasNext();
		}
	}

	public static void merge(File f1, File f2, File merged) throws IOException {
		copyFile(f1, merged);
		Files.write(merged.toPath(), Files.readAllBytes(f2.toPath()), StandardOpenOption.APPEND);
	}

	public static void merge(List<File> files, File merged) throws IOException {
		merged.createNewFile();
		File tmp = new File(merged.getParent(), "tmp");
		tmp.createNewFile();

		try {
			for (File chunk : files) {
				FileUtil.merge(merged, chunk, tmp);
				FileUtils.forceDelete(merged);
				tmp.renameTo(merged);
				tmp = new File(merged.getParent(), "tmp");
				tmp.createNewFile();
			}
		} finally {
			if (tmp != null && tmp.exists())
				FileUtils.deleteQuietly(tmp);
		}

	}

	public static List<File> split(File file, long chunkSize, File destDir) throws IOException {
		RandomAccessFile raf = null;
		List<File> chunks = new ArrayList<File>();
		try {
			raf = new RandomAccessFile(file, "r");
			long numSplits = file.length() / chunkSize;
			long sourceSize = raf.length();
			long remainingBytes = sourceSize - (numSplits * chunkSize);

			NumberFormat nf = new DecimalFormat("00000000000000");
			int maxReadBufferSize = 8 * 1024; // 8KB
			for (int destIx = 1; destIx <= numSplits; destIx++) {
				File chunkFile = new File(destDir, nf.format(destIx));
				BufferedOutputStream bw = new BufferedOutputStream(new FileOutputStream(chunkFile));
				try {
					if (chunkSize > maxReadBufferSize) {
						long numReads = chunkSize / maxReadBufferSize;
						long numRemainingRead = chunkSize % maxReadBufferSize;
						for (int i = 0; i < numReads; i++) {
							readWrite(raf, bw, maxReadBufferSize);
						}
						if (numRemainingRead > 0) {
							readWrite(raf, bw, numRemainingRead);
						}
					} else {
						readWrite(raf, bw, chunkSize);
					}
					chunks.add(chunkFile);
				} finally {
					bw.close();
				}
			}

			if (remainingBytes > 0) {
				File chunkFile = new File(destDir, nf.format(numSplits + 1));
				BufferedOutputStream bw = new BufferedOutputStream(new FileOutputStream(chunkFile));
				readWrite(raf, bw, remainingBytes);
				bw.close();
				chunks.add(chunkFile);
			}
		} finally {
			if (raf != null)
				try {
					raf.close();
				} catch (Throwable t) {
				}
		}
		return chunks;
	}

	static private void readWrite(RandomAccessFile raf, BufferedOutputStream bw, long numBytes) throws IOException {
		byte[] buf = new byte[(int) numBytes];
		int val = raf.read(buf);
		if (val != -1) {
			bw.write(buf);
		}
	}

	public static long countLines(File file) throws IOException {
		InputStream is = new BufferedInputStream(new FileInputStream(file));
		try {
			byte[] c = new byte[1024];
			long count = 0;
			int readChars = 0;
			boolean empty = true;
			while ((readChars = is.read(c)) != -1) {
				empty = false;
				for (int i = 0; i < readChars; ++i) {
					if (c[i] == '\n') {
						++count;
					}
				}
			}
			return (count == 0 && !empty) ? 1 : count;
		} finally {
			is.close();
		}
	}

	/**
	 * Checks if <b>file</b> is in side <b>folder</b> at any level.
	 *
	 * @param folder the folder to inspect
	 * @param file the file to check
	 * 
	 * @return true if <b>file</b> is in side <b>folder</b>
	 */
	public static boolean isInsideFolder(File folder, File file) {
		try {
			return file.getCanonicalPath().startsWith(folder.getCanonicalPath());
		} catch (IOException e) {
			log.error(e.getMessage(), e);
			return false;
		}
	}
}