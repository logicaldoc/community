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
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.List;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.compressors.gzip.GzipCompressorInputStream;
import org.apache.commons.io.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.lingala.zip4j.ZipFile;
import net.lingala.zip4j.exception.ZipException;
import net.lingala.zip4j.model.FileHeader;
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

	public ZipUtil() {
	}

	public ZipUtil(String charset) {
		this.fileNameCharset = charset;
	}

	public List<ZipEntry> listZipEntries(File zipFile) {
		List<ZipEntry> files = new ArrayList<ZipEntry>();

		try (java.util.zip.ZipFile zFile = new java.util.zip.ZipFile(zipFile)) {
			@SuppressWarnings("unchecked")
			Enumeration<ZipEntry> e = (Enumeration<ZipEntry>) zFile.entries();
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
		List<String> files = new ArrayList<String>();
		try {
			ZipFile zFile = new ZipFile(zipFile);
			setCharset(zFile);

			@SuppressWarnings("unchecked")
			List<FileHeader> fileHeaders = zFile.getFileHeaders();
			for (FileHeader fileHeader : fileHeaders)
				files.add(fileHeader.getFileName());
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
	 * @return True if successfully extracted.
	 */
	public boolean unzip(String zipFile, String target) {
		boolean result = true;
		try {
			ZipFile zFile = new ZipFile(zipFile);
			setCharset(zFile);
			zFile.extractAll(target);
		} catch (Throwable e) {
			result = false;
			logError(e.getMessage());
		}
		return result;
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
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try {
			ZipFile zFile = new ZipFile(zipFile);
			setCharset(zFile);
			FileHeader header = zFile.getFileHeader(entry);

			entryStream = zFile.getInputStream(header);
			IOUtils.copy(entryStream, baos);
			baos.flush();
		} catch (Throwable e) {
			logError(e.getMessage());
		} finally {
			try {
				baos.close();
				entryStream.close();
			} catch (Throwable e) {
			}
		}
		return baos.toByteArray();
	}

	/**
	 * This method extracts a specific entry of a zip-file.
	 * 
	 * @param zipFile File to read inside it
	 * @param entry The entry to be read
	 * @param target The extracted file
	 */
	public void unzipEntry(File zipFile, String entry, File target) {
		try (InputStream is = getEntryStream(zipFile, entry);
				OutputStream os = new BufferedOutputStream(new FileOutputStream(target))) {
			is.transferTo(os);
			os.flush();
		} catch (Throwable e) {
			logError(e.getMessage());
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
				// if we reached here, the File object f was not
				// a directory
				// create a FileInputStream on top of f
				FileInputStream fis = new FileInputStream(f);
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
				// close the Stream
				fis.close();
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
		try {
			// create a ZipOutputStream to zip the data to
			ZipOutputStream zos = new ZipOutputStream(new FileOutputStream(dest));

			FileInputStream fis = new FileInputStream(src);
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
			// close the Stream
			fis.close();
			// close the stream
			zos.flush();
			zos.close();
		} catch (Exception e) {
			e.printStackTrace();
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
			e.printStackTrace();
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
	 */
	public void unGZipUnTar(File tarGzFile, File targetDir) {
		File tarFile = null;
		try {
			/*
			 * Ungzip file to extract TAR file.
			 */
			tarFile = File.createTempFile("ungizp", ".tar");
			try (GzipCompressorInputStream archive = new GzipCompressorInputStream(
					new BufferedInputStream(new FileInputStream(tarGzFile)))) {
				OutputStream out = Files.newOutputStream(tarFile.toPath());
				IOUtils.copy(archive, out);
			}

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
		} catch (IOException e) {
			logError(e.getMessage());
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
	 */
	public void unGZip(File gzFile, File targetFile) {
		try {
			unGZip(new FileInputStream(gzFile), targetFile);
		} catch (IOException e) {
			logError(e.getMessage());
		}
	}

	/**
	 * UnGunzips a given .gz stream
	 * 
	 * @param gzStream the .gz stream
	 * 
	 * @param targetFile the target file to unpack to
	 */
	public void unGZip(InputStream gzStream, File targetFile) {
		try {
			/*
			 * Ungzip file to extract TAR file.
			 */
			try (GzipCompressorInputStream archive = new GzipCompressorInputStream(new BufferedInputStream(gzStream))) {
				OutputStream out = Files.newOutputStream(targetFile.toPath());
				IOUtils.copy(archive, out);
			}
		} catch (IOException e) {
			logError(e.getMessage());
		}
	}
}