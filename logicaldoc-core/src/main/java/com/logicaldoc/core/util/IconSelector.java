package com.logicaldoc.core.util;

import org.apache.commons.lang3.StringUtils;

import com.logicaldoc.util.io.FileUtil;

/**
 * utility class to select an icon based on a file extension
 * 
 * @author Sebastian Stein
 */
public class IconSelector {

	private IconSelector() {
	}

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
		ext = normalizeExtension(ext);

		String icon = checkOffice(ext);
		if (StringUtils.isNotEmpty(icon))
			return adaptIfShortcut(shortcut, icon);

		icon = checkMultimedia(ext);
		if (StringUtils.isNotEmpty(icon))
			return adaptIfShortcut(shortcut, icon);

		if (StringUtils.isEmpty(ext))
			icon = "generic.png";
		else if (ext.equals("pdf"))
			icon = "pdf.png";
		else if (isText(ext))
			icon = "text.png";
		else if (isHtml(ext))
			icon = "html.png";
		else if (isEmail(ext))
			icon = "email.png";
		else if (isZip(ext))
			icon = "zip.png";
		else if (isP7M(ext))
			icon = "p7m.png";
		else if (isAutoCad(ext))
			icon = "dwg.png";
		else if (isVector(ext))
			icon = "vector.png";
		else if (isDicom(ext))
			icon = "dcm.png";
		else if (ext.equals("java"))
			icon = "java.png";
		else if (isBook(ext))
			icon = "book.png";
		else
			icon = "generic.png";

		return adaptIfShortcut(shortcut, icon);
	}

	private static String checkOffice(String ext) {
		String icon = "";
		if (isWord(ext))
			icon = "word.png";
		else if (isExcel(ext))
			icon = "excel.png";
		else if (isPowerPoint(ext))
			icon = "powerpoint.png";
		else if (isVisio(ext))
			icon = "visio.png";
		else if (ext.equals("mpp"))
			icon = "project.png";
		return icon;
	}

	private static String checkMultimedia(String ext) {
		String icon = "";
		if (isPicture(ext))
			icon = "picture.png";
		else if (isVideo(ext))
			icon = "film.png";
		else if (isMusic(ext))
			icon = "music.png";
		return icon;
	}

	private static String adaptIfShortcut(boolean shortcut, String icon) {
		if (shortcut)
			icon = FileUtil.getBaseName(icon) + "-sc." + FileUtil.getExtension(icon);
		return icon;
	}

	private static boolean isDicom(String ext) {
		return ext.equals("dcm") || ext.equals("dicom");
	}

	private static boolean isP7M(String ext) {
		return ext.equals("p7m") || ext.equals("m7m");
	}

	private static String normalizeExtension(String ext) {
		if (ext != null)
			ext = ext.contains(".") ? FileUtil.getExtension(ext).toLowerCase() : ext.toLowerCase();
		return ext;
	}

	private static boolean isBook(String ext) {
		return ext.equals("epub") || ext.equals("azw") || ext.equals("azw3") || ext.equals("mobi");
	}

	private static boolean isVisio(String ext) {
		return ext.equals("vsd") || ext.equals("vsdx");
	}

	private static boolean isVector(String ext) {
		return ext.equals("pub") || ext.equals("pubx") || ext.equals("ai") || ext.equals("odg");
	}

	private static boolean isAutoCad(String ext) {
		return ext.equals("dwg") || ext.equals("dxf") || ext.equals("dwt");
	}

	private static boolean isEmail(String ext) {
		return ext.equals("eml") || ext.equals("msg") || ext.equals("mail") || ext.equals("pst");
	}

	private static boolean isHtml(String ext) {
		return ext.equals("htm") || ext.equals("html") || ext.equals("xml") || ext.equals("xhtml");
	}

	private static boolean isMusic(String ext) {
		return ext.equals("mp3") || ext.equals("m4p") || ext.equals("m4a") || ext.equals("wav") || ext.equals("wma")
				|| ext.equals("cda") || ext.equals("wave");
	}

	private static boolean isZip(String ext) {
		return ext.equals("zip") || ext.equals("rar") || ext.equals("gz") || ext.equals("tar") || ext.equals("tgz")
				|| ext.equals("jar") || ext.equals("7z");
	}

	private static boolean isText(String ext) {
		return ext.equals("txt") || ext.equals("properties") || ext.equals("log") || ext.equals("csv")
				|| ext.equals("json");
	}

	private static boolean isWord(String ext) {
		return ext.equals("doc") || ext.equals("docm") || ext.equals("docx") || ext.equals("docxm")
				|| ext.equals("dotx") || ext.equals("dotm") || ext.equals("odt") || ext.equals("rtf")
				|| ext.equals("ott") || ext.equals("sxw") || ext.equals("wpd") || ext.equals("kwd")
				|| ext.equals("dot");
	}

	private static boolean isPowerPoint(String ext) {
		return ext.equals("ppt") || ext.equals("pptm") || ext.equals("pptx") || ext.equals("pptxm") || ext.equals("odp")
				|| ext.equals("pps") || ext.equals("otp") || ext.equals("pot") || ext.equals("sxi")
				|| ext.equals("kpr");
	}

	private static boolean isExcel(String ext) {
		return ext.equals("xls") || ext.equals("xlsm") || ext.equals("xlsb") || ext.equals("xlsx")
				|| ext.equals("xlsxm") || ext.equals("ods") || ext.equals("xlt") || ext.equals("ots")
				|| ext.equals("sxc") || ext.equals("dbf") || ext.equals("ksp") || ext.equals("odb");
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
		return (ext.equals("jpg") || ext.equals("jpeg") || ext.equals("gif") || ext.equals("png") || ext.equals("bmp")
				|| ext.equals("tif") || ext.equals("tiff") || ext.equals("psd") || ext.equals("svg")
				|| ext.equals("jfif") || ext.equals("webp"));
	}
}