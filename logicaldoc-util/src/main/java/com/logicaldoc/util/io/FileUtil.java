package com.logicaldoc.util.io;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.RandomAccessFile;
import java.net.URL;
import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.security.MessageDigest;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.StringTokenizer;
import java.util.stream.Stream;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.LocaleUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.tools.ant.types.selectors.SelectorUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.CollectionUtils;

import com.logicaldoc.util.time.TimeDiff;

/**
 * This class manages I/O operations with files.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 4.0
 */
public class FileUtil {
	private static final String CANNOT_CREATE_FILE = "Cannot create file ";

	static final int BUFF_SIZE = 8192;

	static final byte[] buffer = new byte[BUFF_SIZE];

	private static final Logger log = LoggerFactory.getLogger(FileUtil.class);

	private FileUtil() {
		throw new IllegalStateException("Utility class");
	}

	/**
	 * Writes a stream to a file and then closes the same stream.
	 * 
	 * @param in the input stream
	 * @param filepath the target file path
	 * 
	 * @throws IOException I/O error
	 */
	public static void writeFile(InputStream in, String filepath) throws IOException {
		try (OutputStream os = new FileOutputStream(filepath);) {
			while (true) {
				synchronized (buffer) {
					int amountRead = in.read(buffer);
					if (amountRead == -1)
						break;
					os.write(buffer, 0, amountRead);
				}
			}
			os.flush();
		} finally {
			try {
				if (in != null)
					in.close();
			} catch (IOException e) {
				logError(e.getMessage());
			}
		}
	}

	public static void writeFile(byte[] in, String filepath) throws IOException {
		try (InputStream inStream = new ByteArrayInputStream(in);) {
			writeFile(inStream, filepath);
		}
	}

	public static void writeFile(String text, String filepath) {
		try (FileOutputStream fos = new FileOutputStream(filepath);
				BufferedOutputStream bos = new BufferedOutputStream(fos);) {
			bos.write(text.getBytes(StandardCharsets.UTF_8));
			bos.flush();
		} catch (Exception e) {
			logError(e.getLocalizedMessage());
		}
	}

	public static String readFile(File file) throws IOException {
		try (FileInputStream fisTargetFile = new FileInputStream(file);) {
			return IOUtils.toString(fisTargetFile, StandardCharsets.UTF_8);
		}
	}

	public static String readFile(String filePath) throws IOException {
		return readFile(new File(filePath));
	}

	public static void appendFile(String text, String filepath) {
		try (OutputStream bos = new FileOutputStream(filepath, true);) {
			bos.write(text.getBytes());
			bos.flush();
		} catch (Exception e) {
			logError(e.getLocalizedMessage());
		}
	}

	private static void logError(String message) {
		Logger log = LoggerFactory.getLogger(FileUtil.class);
		log.error(message);
	}

	public static String computeDigest(InputStream is) {
		if (is == null)
			return null;

		String digest = "";
		MessageDigest sha = null;

		try {
			sha = MessageDigest.getInstance("SHA-1");
			byte[] message = new byte[BUFF_SIZE];
			int len = 0;
			while ((len = is.read(message)) != -1) {
				sha.update(message, 0, len);
			}
			byte[] messageDigest = sha.digest();
			// convert the array to String
			int size = messageDigest.length;
			StringBuilder buf = new StringBuilder();
			int unsignedValue = 0;
			String strUnsignedValue = null;
			for (int i = 0; i < size; i++) {
				// convert each messageDigest byte to unsigned
				unsignedValue = messageDigest[i] & 0xff;
				strUnsignedValue = Integer.toHexString(unsignedValue);
				// at least two letters
				if (strUnsignedValue.length() == 1)
					buf.append("0");
				buf.append(strUnsignedValue);
			}
			digest = buf.toString();
			log.debug("Computed Digest: {}", digest);

			return digest;
		} catch (IOException io) {
			log.error("Error generating digest", io);
		} catch (Exception t) {
			log.error("Error generating digest", t);
		} finally {
			try {
				is.close();
			} catch (IOException e) {
				// Nothing to do
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
		try (InputStream is = new BufferedInputStream(new FileInputStream(file), BUFF_SIZE);) {
			return computeDigest(is);
		} catch (Exception e) {
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
		String digest = null;
		try (InputStream is = IOUtils.toInputStream(src, StandardCharsets.UTF_8);) {
			digest = computeDigest(is);
		} catch (IOException e) {
			log.error(e.getMessage());
			digest = null;
		}
		return digest;
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
		out.getParentFile().mkdirs();

		URL resourceUrl = FileUtil.class.getResource(resourceName);
		if (resourceUrl == null)
			resourceUrl = Thread.currentThread().getContextClassLoader().getResource(resourceName);
		if (resourceUrl == null)
			throw new IOException("Resource cannot be found: " + resourceName);

		try (InputStream is = resourceUrl.openStream();
				BufferedInputStream bis = new BufferedInputStream(is);
				OutputStream os = new FileOutputStream(out);
				BufferedOutputStream bos = new BufferedOutputStream(os);) {
			for (;;) {
				int b = is.read();
				if (b == -1)
					break;
				os.write(b);
			}
		}
	}

	/**
	 * Gets the file extension
	 * 
	 * @param fileName name of the file
	 * 
	 * @return the extension
	 */
	public static String getExtension(String fileName) {
		if (fileName != null && fileName.contains(".")) {
			return fileName.substring(fileName.lastIndexOf('.') + 1).toLowerCase();
		} else
			return "";
	}

	/**
	 * Gets the file base name
	 * 
	 * @param fileName name of the file
	 * 
	 * @return the base name
	 */
	public static String getBaseName(String fileName) {
		String name = getName(fileName);

		try {
			return FilenameUtils.getBaseName(name);
		} catch (Exception e) {
			if (name != null && name.contains(".")) {
				return name.substring(0, name.indexOf('.'));
			} else
				return "";
		}
	}

	/**
	 * Gets the file name excluding the path
	 * 
	 * @param fileName name or path of the file
	 * 
	 * @return the file name
	 */
	public static String getName(String fileName) {
		try {
			return FilenameUtils.getName(fileName);
		} catch (Exception e) {
			if (fileName != null && fileName.contains("/"))
				return fileName.substring(fileName.lastIndexOf('/') + 1);
			else if (fileName != null && fileName.contains("\\"))
				return fileName.substring(fileName.lastIndexOf('\\') + 1);
			else
				return fileName;
		}
	}

	/**
	 * Gets the path part
	 * 
	 * @param fileName name or path of the file
	 * 
	 * @return the path
	 */
	public static String getPath(String fileName) {
		try {
			return FilenameUtils.getPath(fileName);
		} catch (Exception e) {
			if (fileName != null && fileName.contains("/"))
				return fileName.substring(0, fileName.lastIndexOf('/'));
			else if (fileName != null && fileName.contains("\\"))
				return fileName.substring(0, fileName.lastIndexOf('\\'));
			else
				return fileName;
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
		Locale locale = LocaleUtils.toLocale("en");
		if (StringUtils.isNotEmpty(language))
			locale = LocaleUtils.toLocale(language);
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
		Locale locale = LocaleUtils.toLocale("en");
		if (StringUtils.isNotEmpty(language))
			locale = LocaleUtils.toLocale(language);
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
	public static boolean matches(String filename, Collection<String> includes, Collection<String> excludes) {
		// First of all check if the filename must be excluded
		boolean matchesExcludes = matchesFilters(filename, excludes);
		if (matchesExcludes)
			return false;

		// Then check if the filename must can be included
		boolean matchesIncludes = matchesFilters(filename, includes);
		if (matchesIncludes)
			return true;

		return CollectionUtils.isEmpty(includes);
	}

	private static boolean matchesFilters(String str, Collection<String> filters) {
		if (!CollectionUtils.isEmpty(filters))
			for (String s : filters)
				if (StringUtils.isNotEmpty(s) && SelectorUtils.match(s, str, false))
					return true;
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
		List<String> inc = new ArrayList<>();
		List<String> exc = new ArrayList<>();

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

		return matches(filename, inc, exc);
	}

	public static void writeUTF8(String content, File file, boolean append) {
		try (BufferedWriter out = new BufferedWriter(
				new OutputStreamWriter(new FileOutputStream(file, append), StandardCharsets.UTF_8));) {
			out.write(content);
		} catch (Exception e) {
			log.error(e.getMessage());
		}
	}

	public static byte[] toByteArray(File file) {
		try (InputStream is = new BufferedInputStream(new FileInputStream(file), 2048);) {
			return IOUtils.toByteArray(is);
		} catch (IOException e) {
			log.error(e.getMessage());
		}
		return new byte[0];
	}

	public static byte[] toByteArray(RandomAccessFile input, long start, long length) throws IOException {
		try (ByteArrayOutputStream output = new ByteArrayOutputStream();) {
			copy(input, output, start, length);
			output.flush();
			return output.toByteArray();
		} finally {
			try {
				input.close();
			} catch (Exception e) {
				// Nothing to do
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

		try (RandomAccessFile inputRa = new RandomAccessFile(input, "r");
				RandomAccessFile outputRa = new RandomAccessFile(output, "rw");
				FileChannel sourceChannel = inputRa.getChannel();
				FileChannel targetChannel = outputRa.getChannel();) {
			targetChannel.transferFrom(sourceChannel, offset, input.length());
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
				toRead -= read;
				if (toRead > 0) {
					output.write(buffer, 0, read);
				} else {
					output.write(buffer, 0, (int) toRead + read);
					break;
				}
			}
		}
	}

	public static void replaceInFile(String sourcePath, String token, String newValue) throws IOException {
		String content = readFile(sourcePath);
		writeFile(content.replace(token, newValue), sourcePath);
	}

	/**
	 * Quickest way to copy a file in Java, makes use of NIO buffer.
	 * 
	 * @param source the source file to copy
	 * @param target the target file
	 * 
	 * @throws IOException if the copy resulted in an error
	 */
	public static void copyFile(File source, File target) throws IOException {
		try (FileInputStream fis = new FileInputStream(source);
				FileChannel in = fis.getChannel();
				FileOutputStream fos = new FileOutputStream(target);
				FileChannel out = fos.getChannel();) {

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
			log.warn(e.getMessage(), e);
		}
	}

	/**
	 * 
	 * Deletes a file doing the best effort
	 * 
	 * @param file the file to delete
	 * @return true inly if the deletion has been done successfully
	 */
	public static boolean delete(File file) {
		if (file == null)
			return true;

		/*
		 * Better to deleteQuitely first because the forceDelete seems to cause
		 * locks at least on Windows when there are frequent deletions. Even
		 * directly using the Windows command rd produces the same behavior
		 */
		Date start = new Date();
		try {
			return FileUtils.deleteQuietly(file);
		} finally {
			if (log.isDebugEnabled())
				log.debug("Deleted path {} in {}", file.getAbsolutePath(), TimeDiff.printDuration(start, new Date()));
		}
	}

	public static void moveQuitely(File source, File target) {
		Date start = new Date();
		try {
			Files.move(source.toPath(), target.toPath(), StandardCopyOption.ATOMIC_MOVE,
					StandardCopyOption.REPLACE_EXISTING);
		} catch (Exception e) {
			log.warn("Cannot move {} into {}", source.getAbsolutePath(), target.getAbsolutePath());
			log.warn(e.getMessage(), e);
		} finally {
			if (log.isDebugEnabled())
				log.debug("Moved path {} into {} in {}", source.getAbsolutePath(), target.getAbsolutePath(),
						TimeDiff.printDuration(start, new Date()));
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
		boolean created = merged.createNewFile();
		if (!created)
			throw new IOException(CANNOT_CREATE_FILE + merged.getAbsolutePath());

		File tmp = new File(merged.getParentFile(), "merge.tmp");

		try {
			created = tmp.createNewFile();
			if (!created)
				throw new IOException(CANNOT_CREATE_FILE + tmp.getAbsolutePath());

			for (File chunk : files) {
				FileUtil.merge(merged, chunk, tmp);
				FileUtils.forceDelete(merged);
				boolean renamed = tmp.renameTo(merged);
				if (!renamed)
					throw new IOException("Cannot rename file to " + merged.getAbsolutePath());

				created = tmp.createNewFile();
				if (!created)
					throw new IOException(CANNOT_CREATE_FILE + tmp.getAbsolutePath());
			}
		} finally {
			FileUtils.deleteQuietly(tmp);
		}

	}

	public static List<File> split(File file, long chunkSize, File destDir) throws IOException {
		List<File> chunks = new ArrayList<>();
		try (RandomAccessFile raf = new RandomAccessFile(file, "r");) {
			long numSplits = file.length() / chunkSize;
			long sourceSize = raf.length();
			long remainingBytes = sourceSize - (numSplits * chunkSize);

			NumberFormat nf = new DecimalFormat("00000000000000");
			int maxReadBufferSize = 8 * 1024; // 8KB
			for (int destIx = 1; destIx <= numSplits; destIx++) {
				File chunkFile = new File(destDir, nf.format(destIx));
				try (FileOutputStream fos = new FileOutputStream(chunkFile);
						BufferedOutputStream bw = new BufferedOutputStream(fos);) {
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
				}
			}

			if (remainingBytes > 0) {
				File chunkFile = new File(destDir, nf.format(numSplits + 1));
				try (FileOutputStream fos = new FileOutputStream(chunkFile);
						BufferedOutputStream bos = new BufferedOutputStream(fos);) {
					readWrite(raf, bos, remainingBytes);
				}
				chunks.add(chunkFile);
			}
		}
		return chunks;
	}

	private static void readWrite(RandomAccessFile raf, BufferedOutputStream bw, long numBytes) throws IOException {
		byte[] buf = new byte[(int) numBytes];
		int val = raf.read(buf);
		if (val != -1) {
			bw.write(buf);
		}
	}

	public static long countLines(File file) throws IOException {
		try (Stream<String> stream = Files.lines(file.toPath(), StandardCharsets.UTF_8)) {
			return stream.count();
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

	/**
	 * Creates an empty file in the default temporary-file directory, using the
	 * given prefix and suffix to generate its name.
	 * 
	 * @param prefix The prefix string to be used in generating the file'sname;
	 *        must be at least three characters longsuffix
	 * @param suffix The suffix string to be used in generating the file'sname;
	 *        may be null, in which case thesuffix ".tmp" will be used
	 * 
	 * @return An abstract pathname denoting a newly-created empty file
	 * 
	 * @throws IOException If the file could not be created
	 */
	public static File createTempFile(String prefix, String suffix) throws IOException {
		return File.createTempFile(prefix, suffix);
	}

	/**
	 * Creates an empty folder in the default temporary-file directory, using
	 * the given prefix to generate its name.
	 * 
	 * @param prefix The prefix string to be used in generating the folder's
	 *        name; must be at least three characters longsuffix
	 * 
	 * @return An abstract pathname denoting a newly-created empty folder
	 * 
	 * @throws IOException If the directory could not be created
	 */
	public static File createTempDirectory(String prefix) throws IOException {
		Path path = Files.createTempDirectory(prefix);
		return path.toFile();
	}
}