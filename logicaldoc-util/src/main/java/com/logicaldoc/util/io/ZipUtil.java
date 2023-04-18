package com.logicaldoc.util.io;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.Context;

import net.lingala.zip4j.ZipFile;
import net.lingala.zip4j.exception.ZipException;
import net.lingala.zip4j.model.FileHeader;
import net.lingala.zip4j.model.LocalFileHeader;
import net.lingala.zip4j.model.ZipParameters;
import net.lingala.zip4j.model.enums.CompressionMethod;

/**
 * This class is for handling with zip-files.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @version 4.0
 */
public class ZipUtil {

	private String fileNameCharset = "UTF-8";

	protected static Logger log = LoggerFactory.getLogger(JarUtil.class);

	/**
	 * Maximum number of entries in the compressed archive, config parameter
	 * zip.maxentires
	 */
	private int maxEntries = 100000;

	/**
	 * Maximum size of the uncompressed contents of the compressed archive,
	 * config parameter zip.maxsize
	 */
	private int maxSize = 1024 * 1024 * 1024; // 1 GB

	/**
	 * Maximum compression ratio, config parameter zip.maxratio
	 */
	private double maxCompressionRatio = 30D;

	public ZipUtil() {
		try {
			maxEntries = Context.get().getProperties().getInt("zip.maxentries", 100000);
			maxSize = Context.get().getProperties().getInt("zip.maxsize", 1024) * 1024 * 1024;
			maxCompressionRatio = Context.get().getProperties().getDouble("zip.maxratio", 30D);
		} catch (Throwable t) {
			// Nothing to do
		}
	}

	public ZipUtil(String charset) {
		this();
		this.fileNameCharset = charset;
	}

	public List<ZipEntry> listZipEntries(File zipFile) {
		List<ZipEntry> files = new ArrayList<>();

		try (java.util.zip.ZipFile zFile = new java.util.zip.ZipFile(zipFile)) {
			Enumeration<? extends ZipEntry> e = zFile.entries();
			while (e.hasMoreElements()) {
				ZipEntry zipEntry = (ZipEntry) e.nextElement();
				files.add(zipEntry);
			}
			files.sort(new Comparator<ZipEntry>() {

				@Override
				public int compare(ZipEntry o1, ZipEntry o2) {
					return o1.getName().compareTo(o2.getName());
				}
			});
		} catch (Throwable e) {
			logError(e.getMessage());
		}

		return files;
	}

	public List<String> listEntries(File zipFile) {
		List<String> files = new ArrayList<>();
		try {
			ZipFile zFile = new ZipFile(zipFile);
			setCharset(zFile);

			List<FileHeader> fileHeaders = zFile.getFileHeaders();
			for (FileHeader fileHeader : fileHeaders) {
				files.add(fileHeader.getFileName());
			}
			
		} catch (Throwable e) {
			logError(e.getMessage());
		}
		return files;
	}

	/**
	 * This method extracts all entries of a zip-file.
	 * 
	 * @param zipFile Path of the zip-file.
	 * @param target Path of the extracted files.
	 * 
	 * @return Number of extracted entries
	 * 
	 * @throws IOException Error unpacking the zip
	 */
	public int unzip(String zipFile, String target) throws IOException {
		return unzip(new FileInputStream(zipFile), target);
	}

	/**
	 * This method extracts all entries of a zip-file.
	 * 
	 * @param zipStream the zip contents
	 * @param target Path of the extracted files.
	 * 
	 * @return Number of extracted entries
	 * 
	 * @throws IOException Error unpacking the zip
	 */
	public int unzip(InputStream zipStream, String target) throws IOException {
		File targetDir = new File(target);
		targetDir.mkdirs();

		int totalSizeArchive = 0;
		int totalEntryArchive = 0;

		LocalFileHeader localFileHeader;
		int readLen;
		byte[] readBuffer = new byte[4096];

		try (net.lingala.zip4j.io.inputstream.ZipInputStream zipInputStream = new net.lingala.zip4j.io.inputstream.ZipInputStream(
				zipStream)) {
			while ((localFileHeader = zipInputStream.getNextEntry()) != null) {
				File extractedFile = new File(targetDir, localFileHeader.getFileName());
				if (localFileHeader.isDirectory()) {
					extractedFile.mkdirs();
				} else {
					try (OutputStream outputStream = new BufferedOutputStream(new FileOutputStream(extractedFile))) {
						while ((readLen = zipInputStream.read(readBuffer)) != -1) {
							outputStream.write(readBuffer, 0, readLen);
							totalSizeArchive += readLen;
						}
					}
				}

				totalEntryArchive++;

				if (totalSizeArchive > maxSize)
					throw new IOException(String.format(
							"Zip file %s looks like a Zip Bomb Attack: the uncompressed data size is over the maximum allowed of %s",
							zipStream, FileUtil.getDisplaySize(maxSize, "en")));

				if (totalEntryArchive > maxEntries)
					throw new IOException(String.format(
							"Zip file %s looks like a Zip Bomb Attack: can lead to inodes exhaustion of the system and is over the maximum allowed of %d",
							zipStream, maxEntries));
			}
		}

		return totalEntryArchive;
	}

	/**
	 * Extracts a specific entry inside a given zip stream
	 * 
	 * @param input the stream of the zip file
	 * @param entry name of the entry to extract
	 * @param target the file where to store the entry
	 * 
	 * @return number of written bytes
	 * 
	 * @throws IOException Error unpacking the zip
	 */
	public long unzip(InputStream input, String entry, File target) throws IOException {
		int totalSizeEntry = 0;
		try (ZipInputStream zis = new ZipInputStream(input);
				BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(target));) {
			ZipEntry ze = zis.getNextEntry();
			while (ze != null) {
				if (ze.getName().equals(entry)) {
					int nBytes = -1;
					byte[] buffer = new byte[4096];

					while ((nBytes = zis.read(buffer)) > 0) {
						bos.write(buffer, 0, nBytes);
						totalSizeEntry += nBytes;

						double compressionRatio = totalSizeEntry / (double) ze.getCompressedSize();
						if (compressionRatio > maxCompressionRatio)
							throw new IOException(String.format(
									"Zip file looks like a Zip Bomb Attack: ratio between compressed and uncompressed data %f is highly suspicious and is over the maximum allowed of %f",
									compressionRatio, maxCompressionRatio));
						if (totalSizeEntry > maxSize)
							throw new IOException(String.format(
									"Zip file looks like a Zip Bomb Attack: the uncompressed data size is over the maximum allowed of %s",
									FileUtil.getDisplaySize(maxSize, "en")));
					}

					bos.flush();
					break;
				}

				ze = zis.getNextEntry();
			}
		}
		return totalSizeEntry;
	}

	/**
	 * This method extracts a specific entry of a zip-file.
	 *
	 * https://github.com/srikanth-lingala/zip4j
	 * 
	 * @param zipFile File to read inside it
	 * @param entry The entry to be read
	 * @param target The extracted file
	 * 
	 * @return number of written bytes
	 * @throws IOException Error extracting the zip
	 */
	public long unzipEntry(File zipFile, String entry, File target) throws IOException {
		if (entry.startsWith("/"))
			entry = entry.substring(1);

		ZipFile zFile = new ZipFile(zipFile);
		setCharset(zFile);
		FileHeader header = zFile.getFileHeader(entry);

		try (InputStream is = zFile.getInputStream(header);
				BufferedInputStream bis = new BufferedInputStream(is);
				FileOutputStream fos = new FileOutputStream(target);
				BufferedOutputStream bos = new BufferedOutputStream(fos);) {

			int nBytes = -1;
			byte[] buffer = new byte[4096];
			int totalSizeEntry = 0;

			while ((nBytes = is.read(buffer)) > 0) {
				bos.write(buffer, 0, nBytes);
				totalSizeEntry += nBytes;

				double compressionRatio = totalSizeEntry / (double) header.getCompressedSize();
				if (compressionRatio > maxCompressionRatio)
					throw new IOException(String.format(
							"Zip file %s looks like a Zip Bomb Attack: ratio between compressed and uncompressed data %f is highly suspicious and is over the maximum allowed of %f",
							zipFile.getAbsolutePath(), compressionRatio, maxCompressionRatio));
				if (totalSizeEntry > maxSize)
					throw new IOException(String.format(
							"Zip file %s looks like a Zip Bomb Attack: the uncompressed data size is over the maximum allowed of %s",
							zipFile.getAbsolutePath(), FileUtil.getDisplaySize(maxSize, "en")));
			}

			bos.flush();
			return totalSizeEntry;
		}
	}

	/**
	 * Read the entry inside the file zip resource.
	 * 
	 * @param zipFile File to read inside it
	 * @param entry The entry to be read
	 * 
	 * @return The bytes of the entry
	 */
	public byte[] getEntryBytes(File zipFile, String entry) {
		if (entry.startsWith("/"))
			entry = entry.substring(1);

		InputStream entryStream = null;
		try (ByteArrayOutputStream baos = new ByteArrayOutputStream();) {
			ZipFile zFile = new ZipFile(zipFile);
			setCharset(zFile);
			FileHeader header = zFile.getFileHeader(entry);

			entryStream = zFile.getInputStream(header);
			IOUtils.copy(entryStream, baos);
			baos.flush();
			return baos.toByteArray();
		} catch (Throwable e) {
			logError(e.getMessage());
			return null;
		} finally {
			try {
				if (entryStream != null)
					entryStream.close();
			} catch (Throwable e) {
				// Nothing to do
			}
		}
	}

	private void setCharset(ZipFile zipFile) throws ZipException {
		if (fileNameCharset != null && !"auto".equals(fileNameCharset))
			zipFile.setCharset(Charset.forName(fileNameCharset));
	}

	/**
	 * Read the entry inside the file zip resource.
	 * 
	 * @param zipFile File to read inside it
	 * @param entry The entry to be read
	 * @return The stream of the entry
	 */
	public InputStream getEntryStream(File zipFile, String entry) {
		if (entry.startsWith("/"))
			entry = entry.substring(1);

		try {
			ZipFile zFile = new ZipFile(zipFile);
			setCharset(zFile);
			FileHeader header = zFile.getFileHeader(entry);
			return zFile.getInputStream(header);
		} catch (Throwable e) {
			logError(e.getMessage());
			return null;
		}
	}

	public String getEntryContent(File zip, String entry) throws FileNotFoundException, IOException {
		return IOUtil.getStringFromInputStream(getEntryStream(zip, entry));
	}

	private static void logError(String message) {
		Logger logger = LoggerFactory.getLogger(ZipUtil.class);
		logger.error(message);
	}

	public static void addEntry(File zip, String entry, InputStream content) {
		try (ZipFile zipFile = new ZipFile(zip)) {
			// read war.zip and write to

			ZipParameters parameters = new ZipParameters();
			parameters.setCompressionMethod(CompressionMethod.DEFLATE);
			// this would be the name of the file for this entry in the zip file
			parameters.setFileNameInZip(entry);

			// Creates a new entry in the zip file and adds the content to the
			// zip file
			zipFile.addStream(content, parameters);
		} catch (Exception e) {
			logError(e.getMessage());
		}
	}

	private void zipDir(File zipDir, ZipOutputStream zos, File startZipDir) {
		try {
			// get a listing of the directory content
			File[] dirList = zipDir.listFiles();
			byte[] readBuffer = new byte[2156];
			int bytesIn = 0;
			// loop through dirList, and zip the files
			for (int i = 0; i < dirList.length; i++) {
				File f = dirList[i];
				if (f.isDirectory()) {
					// if the File object is a directory, call this
					// function again to add its content recursively
					zipDir(f, zos, startZipDir);
					// loop again
					continue;
				}

				// if we reached here, the File object f was not a directory
				// create a FileInputStream on top of f

				try (FileInputStream fis = new FileInputStream(f);) {
					// create a new zip entry
					String path = f.getPath();
					if (!path.equals(startZipDir.getPath()))
						path = path.substring(startZipDir.getPath().length());
					if (path.startsWith(File.separator))
						path = path.substring(1);
					ZipEntry anEntry = new ZipEntry(path);
					// place the zip entry in the ZipOutputStream object
					zos.putNextEntry(anEntry);
					// now write the content of the file to the ZipOutputStream
					while ((bytesIn = fis.read(readBuffer)) != -1) {
						zos.write(readBuffer, 0, bytesIn);
					}
				}
			}
		} catch (Exception e) {
			logError(e.getMessage());
		}
	}

	/**
	 * Compress a single file
	 * 
	 * @param src The source file
	 * @param dest The destination archive file
	 */
	public void zipFile(File src, File dest) {

		try (ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(dest));
				FileInputStream fis = new FileInputStream(src);) {
			// create a new zip entry
			ZipEntry anEntry = new ZipEntry(src.getName());
			// place the zip entry in the ZipOutputStream object
			zos.putNextEntry(anEntry);

			byte[] readBuffer = new byte[2156];
			int bytesIn = 0;

			// now write the content of the file to the ZipOutputStream
			while ((bytesIn = fis.read(readBuffer)) != -1) {
				zos.write(readBuffer, 0, bytesIn);
			}
			// close the stream
			zos.flush();
		} catch (Exception e) {
			logError(e.getMessage());
		}
	}

	/**
	 * Zips a folder into a .zip archive
	 * 
	 * @param inFolder the folder to compress
	 * @param outFile the zip file
	 */
	public void zipFolder(File inFolder, File outFile) {
		try {
			// create a ZipOutputStream to zip the data to
			ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(outFile));
			// assuming that there is a directory named inFolder (If there
			// isn't create one) in the same directory as the one the code
			// runs from,
			// call the zipDir method
			zipDir(inFolder, zos, inFolder);
			// close the stream
			zos.flush();
			zos.close();
		} catch (Exception e) {
			logError(e.getMessage());
		}
	}

	public String getFileNameCharset() {
		return fileNameCharset;
	}

	public void setFileNameCharset(String fileNameCharset) {
		this.fileNameCharset = fileNameCharset;
	}

	/**
	 * Gunzips and Untars a given .tar.gz file
	 * 
	 * @param tarGzFile the .tar.gz file
	 * @param targetDir the target folder to unpack to
	 * 
	 * @throws IOException An error processing the GZip or Tar
	 */
	public void unGZipUnTar(File tarGzFile, File targetDir) throws IOException {
		File tarFile = null;
		try {
			/*
			 * Ungzip file to extract TAR file.
			 */
			tarFile = FileUtil.createTempFile("ungizp", ".tar");
			unGZip(tarGzFile, tarFile);

			/*
			 * Untar extracted TAR file
			 */
			try (FileInputStream tarIs = new FileInputStream(tarFile);
					BufferedInputStream tarBIS = new BufferedInputStream(tarIs);
					TarArchiveInputStream archive = new TarArchiveInputStream(tarBIS)) {

				TarArchiveEntry entry;
				while ((entry = archive.getNextTarEntry()) != null) {
					File file = new File(targetDir + "/" + entry.getName());
					if (!entry.isFile()) {
						file.mkdirs();
						file.mkdir();
					} else {
						try (FileOutputStream fos = new FileOutputStream(file)) {
							IOUtils.copy(archive, fos);
							fos.flush();
						}
					}
				}
			}
		} finally {
			if (tarFile != null && tarFile.exists())
				FileUtil.strongDelete(tarFile);
		}
	}

	/**
	 * UnGunzips a given .gz file
	 * 
	 * @param gzFile the .gz file
	 * @param targetFile the target file to unpack to
	 *
	 * @return size of the written file
	 * 
	 * @throws IOException Error processing the GZip
	 */
	public long unGZip(File gzFile, File targetFile) throws IOException {
		return unGZip(new FileInputStream(gzFile), targetFile);
	}

	/**
	 * UnGunzips a given .gz stream
	 * 
	 * @param is the .gz stream
	 * 
	 * @param target the target file to unpack to
	 * 
	 * @return size of the written file
	 * 
	 * @throws IOException Error processing the GZip
	 */
	public long unGZip(InputStream is, File target) throws IOException {
		try (GzipCompressorInputStream archive = new GzipCompressorInputStream(new BufferedInputStream(is));
				FileOutputStream fos = new FileOutputStream(target);
				BufferedOutputStream bos = new BufferedOutputStream(fos);) {

			int nBytes = -1;
			byte[] buffer = new byte[4096];
			int totalSizeEntry = 0;

			while ((nBytes = archive.read(buffer)) > 0) {
				bos.write(buffer, 0, nBytes);
				totalSizeEntry += nBytes;

				if (totalSizeEntry > maxSize)
					throw new IOException(String.format(
							"GZip file looks like a Zip Bomb Attack: the uncompressed data size is over the maximum allowed of %s",
							FileUtil.getDisplaySize(maxSize, "en")));
			}

			bos.flush();
			return totalSizeEntry;
		}
	}
}