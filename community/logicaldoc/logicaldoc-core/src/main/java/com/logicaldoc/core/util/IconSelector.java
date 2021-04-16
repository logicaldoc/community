package com.logicaldoc.core.util;

import org.apache.commons.io.FilenameUtils;

/**
 * utility class to select an icon based on a file extension
 * 
 * @author Sebastian Stein
 */
public class IconSelector {

	/**
	 * Returns the icon by parsing the provided file extension
	 * 
	 * @param ext file extension
	 * 
	 * @return file name of the icon
	 */
	public static String selectIcon(String ext) {
		return selectIcon(ext, false);
	}

	/**
	 * Returns the icon by parsing the provided file extension
	 * 
	 * @param ext the file extension(or file name)
	 * @param shortcut If the icon displays a shortcut
	 * 
	 * @return the icon file name
	 */
	public static String selectIcon(String ext, boolean shortcut) {
		String icon = "";
		if (ext != null)
			ext = ext.contains(".") ? FilenameUtils.getExtension(ext).toLowerCase() : ext.toLowerCase();

		if (ext == null || ext.equalsIgnoreCase(""))
			icon = "generic.png";
		else if (ext.equals("pdf"))
			icon = "pdf.png";
		else if (ext.equals("txt") || ext.equals("properties") || ext.equals("log") || ext.equals("csv")
				|| ext.equals("json"))
			icon = "text.png";
		else if (ext.equals("doc") || ext.equals("docm") || ext.equals("docx") || ext.equals("docxm")
				|| ext.equals("dotm") || ext.equals("odt") || ext.equals("rtf") || ext.equals("ott")
				|| ext.equals("sxw") || ext.equals("wpd") || ext.equals("kwd") || ext.equals("dot"))
			icon = "word.png";
		else if (ext.equals("xls") || ext.equals("xlsm") || ext.equals("xlsb") || ext.equals("xlsx")
				|| ext.equals("xlsxm") || ext.equals("ods") || ext.equals("xlt") || ext.equals("ots")
				|| ext.equals("sxc") || ext.equals("dbf") || ext.equals("ksp") || ext.equals("odb"))
			icon = "excel.png";
		else if (ext.equals("ppt") || ext.equals("pptm") || ext.equals("pptx") || ext.equals("pptxm")
				|| ext.equals("odp") || ext.equals("pps") || ext.equals("otp") || ext.equals("pot") || ext.equals("sxi")
				|| ext.equals("kpr"))
			icon = "powerpoint.png";
		else if (isPicture(ext))
			icon = "picture.png";
		else if (ext.equals("htm") || ext.equals("html") || ext.equals("xml") || ext.equals("xhtml"))
			icon = "html.png";
		else if (ext.equals("eml") || ext.equals("msg") || ext.equals("mail"))
			icon = "email.png";
		else if (ext.equals("zip") || ext.equals("rar") || ext.equals("gz") || ext.equals("tar") || ext.equals("jar")
				|| ext.equals("7z"))
			icon = "zip.png";
		else if (ext.equals("p7m") || ext.equals("m7m"))
			icon = "p7m.png";
		else if (ext.equals("dwg") || ext.equals("dxf") || ext.equals("dwt"))
			icon = "dwg.png";
		else if (isVideo(ext))
			icon = "film.png";
		else if (ext.equals("mp3") || ext.equals("m4p") || ext.equals("m4a") || ext.equals("wav") || ext.equals("wma")
				|| ext.equals("cda") || ext.equals("wave"))
			icon = "music.png";
		else if (ext.equals("vsd") || ext.equals("vsdx") || ext.equals("pub") || ext.equals("pubx") || ext.equals("ai"))
			icon = "vector.png";
		else if (ext.equals("dcm") || ext.equals("dicom"))
			icon = "dcm.png";
		else if (ext.equals("java"))
			icon = "java.png";
		else if (ext.equals("epub") || ext.equals("azw") || ext.equals("azw3") || ext.equals("mobi"))
			icon = "book.png";
		else
			icon = "generic.png";

		if (shortcut)
			icon = FilenameUtils.getBaseName(icon) + "-sc." + FilenameUtils.getExtension(icon);

		return icon;
	}

	/**
	 * Determines if the given extension represents a video
	 * 
	 * @param ext the file extension
	 * 
	 * @return if ext is a video
	 */
	private static boolean isVideo(String ext) {

		return (ext.equals("avi") || ext.equals("mpg") || ext.equals("mp4") || ext.equals("mov") || ext.equals("wmv")
				|| ext.equals("mkv") || ext.equals("mpeg") || ext.equals("m4v") || ext.equals("divx")
				|| ext.equals("flv") || ext.equals("m2v") || ext.equals("m2ts"));
	}

	/**
	 * Determines if the given extension represents a picture
	 * 
	 * @param ext the file extension
	 * 
	 * @return if ext is an image
	 */
	public static boolean isPicture(String ext) {

		if (ext != null)
			ext = ext.toLowerCase();

		return (ext.equals("jpg") || ext.equals("jpeg") || ext.equals("gif") || ext.equals("png") || ext.equals("bmp")
				|| ext.equals("tif") || ext.equals("tiff") || ext.equals("psd") || ext.equals("svg")
				|| ext.equals("jfif") || ext.equals("webp"));
	}
}