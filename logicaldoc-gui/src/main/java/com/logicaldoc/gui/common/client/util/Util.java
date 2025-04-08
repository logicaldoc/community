package com.logicaldoc.gui.common.client.util;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.google.gwt.core.client.GWT;
import com.google.gwt.http.client.Request;
import com.google.gwt.http.client.RequestBuilder;
import com.google.gwt.http.client.RequestCallback;
import com.google.gwt.http.client.RequestException;
import com.google.gwt.http.client.Response;
import com.google.gwt.http.client.URL;
import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.NumberFormat;
import com.google.gwt.user.client.Random;
import com.google.gwt.user.client.Timer;
import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.CookiesManager;
import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIInfo;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.controllers.FolderController;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.EventPanel;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.common.client.widgets.ApplicationRestarting;
import com.logicaldoc.gui.common.client.widgets.ToastNotification;
import com.smartgwt.client.widgets.Canvas;
import com.smartgwt.client.widgets.layout.Layout;

/**
 * General utilities container for the GUI
 * 
 * @author Marco Meschieri - LogicalDOC
 * 
 * @since 8.8.3
 */
public abstract class Util {

	private static final String HEIGHT = "&height=";

	private static final String WIDTH = "' width='";

	private static final String PX_HEIGHT = "px' height='";

	private static final String IMG_SRC = "<img src='";

	private static final String FORMAT_PATTERN_ONE_DIGIT = "###.#";

	private static final String FORMAT_PATTERN_TWO_DIGITS = "###.##";

	private static final String FORMAT_PATTERN_BYTES = "#,###";

	private static final String AND_LOCALE_EQUAL = "&locale=";

	private static final String END_DIV = "</div>";

	private static final String NBSP = "&nbsp;";

	private static final String DIV_CLASS_BOX = "<div class='box'>";

	private static final String AND_SID_EQUAL = "&sid=";

	private static final String AND_FILEVERSION_EQUAL = "&fileVersion=";

	private static final Set<String> officeExts = new HashSet<>(Arrays.asList(".doc", ".xls", ".xlsm", ".ppt", ".docx",
			".docxm", ".dotm", ".xlsx", ".pptx", ".rtf", ".odt", ".ods", ".odp", ".vsd", ".vsdx", ".mpp"));

	private static final Set<String> spreadsheetExts = new HashSet<>(Arrays.asList(".xls", ".xlsm", ".xlsx", ".ods"));

	private static final Set<String> presentationExts = new HashSet<>(Arrays.asList(".ppt", ".pptx", ".pptm", ".odp"));

	private static final Set<String> imageExts = new HashSet<>(
			Arrays.asList(".gif", ".jpg", ".jpeg", ".bmp", ".tif", ".tiff", ".png", ".jfif", ".webp"));

	private static final Set<String> videoExts = new HashSet<>(
			Arrays.asList(".mp4", ".avi", ".mpg", ".wmv", ".wma", ".asf", ".mov", ".rm", ".flv", ".aac", ".vlc", ".ogg",
					".webm", ".swf", ".mpeg", ".swf", ".m2v", ".m2ts", ".mkv", ".m4v"));

	private static final Set<String> audioExts = new HashSet<>(Arrays.asList(".mp3", ".m4p", ".m4a", ".wav"));

	private static final Set<String> webcontentExts = new HashSet<>(Arrays.asList(".html", ".htm", ".xhtml"));

	private static final Set<String> emailExts = new HashSet<>(Arrays.asList(".eml", ".msg"));

	private static final Set<String> dicomExts = new HashSet<>(Arrays.asList(".dcm", ".dicom"));

	private Util() {
		// Empty constructor
	}

	/**
	 * Generates HTML image code with style.
	 * 
	 * @param imageName the name of the icon image
	 * @param alt the image alt
	 * @param style CSS style specification
	 * 
	 * @return the resultant HTML
	 */
	public static String imageHTML(String imageName, String alt, String style) {
		return "<img border=\"0\" align=\"absmidle\" alt=\"" + (alt != null ? alt : "") + "\" title=\""
				+ (alt != null ? alt : "") + "\"" + (style != null ? "style='" + style + "'" : "") + " src='"
				+ Util.imageUrl(imageName) + "' />";
	}

	public static String imageHTML(String imageName, Integer width, Integer height, String style) {
		String html = "<img border='0' alt='' title='' src='" + Util.imageUrl(imageName) + "' ";
		if (width != null)
			html += " width='" + width + "px' ";
		if (height != null)
			html += " height='" + height + "px' ";
		if (style != null)
			html += " style='" + style + "' ";
		html += " />";
		return html;
	}

	public static String downloadAttachmentURL(long docId, String fileVersion, String attachmentFileName) {
		String url = contextPath() + "download-attachment?docId=" + docId;
		if (fileVersion != null)
			url += AND_FILEVERSION_EQUAL + fileVersion;
		if (attachmentFileName != null)
			url += "&attachmentFileName=" + URL.encode(attachmentFileName);
		return url;
	}

	public static String downloadURL(long docId, String fileVersion, String suffix, boolean open) {
		String url = contextPath() + "download?docId=" + docId;
		if (fileVersion != null)
			url += AND_FILEVERSION_EQUAL + fileVersion;
		if (suffix != null)
			url += "&suffix=" + suffix;
		if (open)
			url += "&open=true";
		return url;
	}

	public static String downloadURL(long docId, String fileVersion, boolean open) {
		return downloadURL(docId, fileVersion, null, open);
	}

	public static String downloadURL(long docId, String fileVersion) {
		return downloadURL(docId, fileVersion, false);
	}

	public static String downloadPdfURL(long docId, String fileVersion) {
		String url = Util.contextPath() + "convertpdf?docId=" + docId;
		if (fileVersion != null && !fileVersion.isEmpty())
			url += "&version=" + fileVersion;
		return url;
	}

	public static String downloadURL(long docId) {
		return downloadURL(docId, null, false);
	}

	public static void download(String urlToRedirectTo) {
		uninstallCloseWindowAlert();
		WindowUtils.openUrl(urlToRedirectTo);
		installCloseWindowAlert();
	}

	public static String downloadTicketURL(String ticketId) {
		return Util.contextPath() + "download-ticket?ticketId=" + ticketId;
	}

	public static void downloadTicket(String ticketId) {
		download(downloadTicketURL(ticketId));
	}

	public static String qrURL(String content, int size) {
		return Util.contextPath() + "barcode?label=false&format=QR_CODE&width=" + size + HEIGHT + size + "&code="
				+ content;
	}

	public static String qrImg(String content, int size) {
		if (isCommunity())
			return "";
		else
			return IMG_SRC + qrURL(content, size) + WIDTH + size + "' />";
	}

	public static String displayURL(Long docId, Long folderId) {
		String url = contextPath() + "display?";
		if (docId != null)
			url += Constants.DOC_ID + "=" + docId;
		else
			url += Constants.FOLDER_ID + "=" + folderId;
		return url;
	}

	public static String webEditorUrl(long docId, String fileName, int height) {
		return contextPath() + "ckeditor/index.jsp?docId=" + docId + "&lang=" + I18N.getLocale() + "&fileName="
				+ fileName + HEIGHT + height + AND_SID_EQUAL + Session.get().getSid();
	}

	public static String webEditorUrl(int height) {
		return contextPath() + "ckeditor/index.jsp?docId=nodoc&lang=" + I18N.getLocale() + HEIGHT + height
				+ AND_SID_EQUAL + Session.get().getSid();
	}

	public static String webstartURL(String appName, Map<String, String> params) {
		StringBuilder url = new StringBuilder(GWT.getHostPageBaseURL());
		url.append("webstart/");
		url.append(appName);
		url.append(".jsp?random=");
		url.append(Random.nextInt());
		url.append("&language=");
		url.append(I18N.getLocale());
		url.append("&docLanguage=");
		url.append(I18N.getDefaultLocaleForDoc());
		url.append(AND_SID_EQUAL);
		url.append(Session.get().getSid());

		if (params != null)
			for (Entry<String, String> entry : params.entrySet()) {
				url.append("&");
				url.append(entry.getKey());
				url.append("=");
				url.append(URL.encode(entry.getValue()));
			}

		return url.toString();
	}

	/**
	 * Generates HTML code for reproducing video files
	 * 
	 * @param mediaUrl URL of the media file to reproduce
	 * @param width width specification
	 * @param height height specification
	 * 
	 * @return the HTML content
	 */
	public static String videoHTML(String mediaUrl, String width, String height) {
		String tmp = "<video controls ";
		if (width != null)
			tmp += "width='" + width + "' ";
		if (height != null)
			tmp += "height='" + height + "' ";
		tmp += ">";
		tmp += "<source src='" + mediaUrl + "' />";
		tmp += "</video>";
		return tmp;
	}

	/**
	 * Generates HTML code for reproducing audio files
	 * 
	 * @param mediaUrl URL of the media file to reproduce
	 * 
	 * @return the HTML content
	 */
	public static String audioHTML(String mediaUrl) {
		String tmp = "<audio style='margin-top: 20px; vertical-align: middle; text-align: center' controls >";
		tmp += "<source src='" + mediaUrl + "' />";
		tmp += "</audio>";
		return tmp;
	}

	public static String thumbnailUrl(long docId, String fileVersion) {
		String url = GWT.getHostPageBaseURL() + "thumbnail?docId=" + docId + "&random=" + new Date().getTime();
		if (fileVersion != null)
			url += AND_FILEVERSION_EQUAL + fileVersion;
		return url;
	}

	public static String thumbnailImgageHTML(long docId, String fileVersion, Integer width, Integer height) {
		String style = "";
		if (width != null)
			style += "width:" + width + "px; ";
		if (height != null)
			style += "height:" + height + "px; ";

		return IMG_SRC + thumbnailUrl(docId, fileVersion) + "' style='" + style + "' />";
	}

	public static String tileUrl(long docId, String fileVersion) {
		return thumbnailUrl(docId, fileVersion) + "&suffix=tile.png";
	}

	public static String tileImageHTML(long docId, String fileVersion, Integer width, Integer height) {
		String style = "";
		if (width != null)
			style += "width:" + width + "px; ";
		if (height != null)
			style += "height:" + height + "px; ";

		return IMG_SRC + tileUrl(docId, fileVersion) + "' style='" + style + "' />";
	}

	public static String imageUrl(String imageName) {
		return imagePrefix() + imageName;
	}

	public static String fileIconUrl(String iconName) {
		return imagePrefix() + "FileIcons/" + iconName;
	}

	public static String fileNameIcon(String iconName, int size) {

		StringBuilder sb = new StringBuilder("<div class='icon-container filenameIcon'>");

		if (iconName.contains("-")) {
			String baseIconName = iconName.substring(0, iconName.indexOf('-'));

			if (iconName.contains("-shortcut")) {
				long shortcutSize = Math.round(size * 0.625D);
				long shortcutMarginTop = Math.round(size * 0.4375D);
				long shortcutMarginLeft = Math.round(size * 0.6D);
				sb.append(IMG_SRC + fileIconUrl("shortcut.svg") + WIDTH + shortcutSize + PX_HEIGHT + shortcutSize
						+ "px' style='position:absolute; z-index:1; margin-top: " + shortcutMarginTop
						+ "px; margin-left:" + shortcutMarginLeft + "px'/>");
			}

			if (iconName.contains("-clip")) {
				long clipSize = Math.round(size * 0.625D);
				long clipMarginTop = Math.round(size * 0.125D);
				long clipMarginLeft = 0;
				sb.append(IMG_SRC + fileIconUrl("clip.svg") + WIDTH + clipSize + PX_HEIGHT + clipSize
						+ "px' style='position:absolute; z-index:2; margin-top: " + clipMarginTop + "px; margin-left:"
						+ clipMarginLeft + "px'/>");
			}

			iconName = baseIconName;
		}

		sb.append("<img class='filenameIcon' src='" + fileIconUrl(iconName + ".svg") + WIDTH + size + PX_HEIGHT + size
				+ "px' />");

		sb.append(END_DIV);
		return sb.toString();
	}

	public static String iconWithFilename(String iconName, String fileName) {
		return DIV_CLASS_BOX + fileNameIcon(iconName, 16) + NBSP + fileName + END_DIV;
	}

	public static String avatarWithText(String userIdOrName, String text) {
		return DIV_CLASS_BOX + avatarImg(userIdOrName, 16) + NBSP + text + END_DIV;
	}

	public static String avatarWithText(Long userId, String text) {
		return DIV_CLASS_BOX + avatarImg(userId != null ? "" + userId : "0", 16) + NBSP + text + END_DIV;
	}

	public static String textWithAvatar(Long userId, String text) {
		return DIV_CLASS_BOX + text + NBSP + avatarImg(userId != null ? "" + userId : "0", 16) + END_DIV;
	}

	public static String avatarImg(Long userId, int size) {
		return avatarImg(userId != null ? "" + userId : "0", size);
	}

	public static String avatarImg(String userIdOrName, int size) {
		String url = userAvatarUrl(userIdOrName != null ? "" + userIdOrName : "0", false);
		return "<img class='avatarIcon' src='" + url + WIDTH + size + PX_HEIGHT + size + "px' />";
	}

	public static String avatarUrl(long userId) {
		return userAvatarUrl("" + userId, false);
	}

	public static String avatarUrl(String userIdOrName) {
		return userAvatarUrl(userIdOrName, false);
	}

	public static String userAvatarUrl(String userIdOrName, boolean avoidCaching) {
		return GWT.getHostPageBaseURL() + "avatar?userId=" + userIdOrName.trim()
				+ (avoidCaching ? "&random=" + new Date().getTime() : "");
	}

	public static String licenseUrl() {
		return contextPath() + "license";
	}

	public static String websocketUrl() {
		String url = contextPath() + "wk-event";
		if (url.toLowerCase().startsWith("https"))
			return "wss" + url.substring(url.indexOf(':'));
		else
			return "ws" + url.substring(url.indexOf(':'));
	}

	public static String strip(String src) {
		if (src == null)
			return null;
		else
			return src.replaceAll("\\<.*\\>", "").replace(NBSP, " ");
	}

	public static String toString(Object[] elements) {
		if (elements == null || elements.length == 0)
			return null;

		try {
			String str = Arrays.asList(elements).toString();
			return str.substring(1, str.length() - 1);
		} catch (Exception e) {
			return null;
		}
	}

	public static String contextPath() {
		String url = GWT.getModuleBaseURL().replace(GWT.getModuleName() + "/", "");
		if (!url.endsWith("/"))
			url += "/";
		return url;
	}

	public static String currentSkin() {
		String skin = null;
		try {
			if (Session.get() != null && Session.get().getInfo() != null
					&& Session.get().getInfo().getBranding() != null)
				skin = Session.get().getInfo().getBranding().getSkin();
			if (skin == null)
				skin = detectSkin();
		} catch (Exception t) {
			// Nothing to do
		}

		if (skin == null)
			skin = "Tahoe";
		return skin;
	}

	public static String imagePrefix() {
		String base = GWT.getModuleBaseURL();
		if (!base.endsWith("/"))
			base = base + "/";
		return base + "sc/skins/" + currentSkin() + "/images/";
	}

	/**
	 * Generates HTML image code.
	 * 
	 * @param imageName the image name
	 * 
	 * @return the resultant HTML
	 */
	public static String imageHTML(String imageName) {
		return imageHTML(imageName, "", null);
	}

	public static boolean isCommunity() {
		return !Feature.enabled(Feature.ADDITIONAL_FORMATS);
	}

	public static boolean isCommercial() {
		return Feature.enabled(Feature.ADDITIONAL_FORMATS);
	}

	public static boolean isOfficeFileType(String type) {
		return officeExts.stream().anyMatch(type::equalsIgnoreCase);
	}

	public static boolean isOfficeFile(String fileName) {
		return officeExts.stream().anyMatch(ext -> fileName.toLowerCase().endsWith(ext));
	}

	public static boolean isSpreadsheetFile(String fileName) {
		return spreadsheetExts.stream().anyMatch(ext -> fileName.toLowerCase().endsWith(ext));
	}

	public static boolean isPresentationFile(String fileName) {
		return presentationExts.stream().anyMatch(ext -> fileName.toLowerCase().endsWith(ext));
	}

	public static boolean isDICOMFile(String fileName) {
		return dicomExts.stream().anyMatch(ext -> fileName.toLowerCase().endsWith(ext));
	}

	public static boolean isTextFile(String fileName) {
		String[] exts = new String[] { "txt" };
		String extensions = Session.get().getConfig("gui.text.extensions");
		if (extensions != null && !extensions.isEmpty()) {
			exts = extensions.trim().toLowerCase().split(",");
			for (int i = 0; i < exts.length; i++) {
				exts[i] = exts[i].trim();
				if (!exts[i].startsWith("."))
					exts[i] = "." + exts[i];
			}
		}

		String tmp = fileName.toLowerCase();
		for (String ext : exts) {
			if (tmp.endsWith(ext))
				return true;
		}
		return false;
	}

	public static boolean isImageFile(String fileName) {
		return imageExts.stream().anyMatch(ext -> fileName.toLowerCase().endsWith(ext));
	}

	public static boolean isWebContentFile(String fileName) {
		return webcontentExts.stream().anyMatch(ext -> fileName.toLowerCase().endsWith(ext));
	}

	public static boolean isMediaFile(String fileName) {
		return videoExts.stream().anyMatch(ext -> fileName.toLowerCase().endsWith(ext)) || isAudioFile(fileName);
	}

	public static boolean isAudioFile(String fileName) {
		return audioExts.stream().anyMatch(ext -> fileName.toLowerCase().endsWith(ext));
	}

	public static boolean isEmailFile(String fileName) {
		return emailExts.stream().anyMatch(ext -> fileName.toLowerCase().endsWith(ext));
	}

	/**
	 * Copies a text into the client's clipboard
	 * 
	 * @param text the content to put into the clipboards
	 */
	public static void copyText(String text) {
		writeToClipboard(text);
		GuiLog.info(I18N.message("texthascopied"));
	}

	/**
	 * Writes a text into the client's clipboard
	 * 
	 * @param text the content to put into the clipboards
	 */
	public static native void writeToClipboard(String text) /*-{		
		$wnd.copy(text);
	}-*/;

	/**
	 * Format file size in Bytes, KBytes, MBytes or GBytes.
	 * 
	 * @param size The file size in bytes S
	 * @return The formated file size
	 */
	public static native String formatSize(double size) /*-{
		if (size / 1024 < 1) {
			str = size + " Bytes";
		} else if (size / 1048576 < 1) {
			str = (size / 1024).toFixed(1) + " KBytes";
		} else if (size / 1073741824 < 1) {
			str = (size / 1048576).toFixed(1) + " MBytes";
		} else {
			str = (size / 1073741824).toFixed(1) + " GBytes";
		}
		return str;
	}-*/;

	/**
	 * Format file size in Bytes, KB, MB, GB.
	 * 
	 * @param size The file size in bytes.
	 * @return The formated file size.
	 */
	public static native String formatSizeCompact(double size) /*-{
		if (size / 1024 < 1) {
			str = size + " Bytes";
		} else if (size / 1048576 < 1) {
			str = (size / 1024).toFixed(1) + " KB";
		} else if (size / 1073741824 < 1) {
			str = (size / 1048576).toFixed(1) + " MB";
		} else {
			str = (size / 1073741824).toFixed(1) + " GB";
		}
		return str;
	}-*/;

	public static String formatLong(Long number) {
		String str = "";
		if (number == null)
			return str;
		NumberFormat fmt = NumberFormat.getFormat(FORMAT_PATTERN_BYTES);
		str = fmt.format(number);
		str = str.replace(',', I18N.groupingSepator());
		return str;
	}

	public static String formatInt(Integer number) {
		String str = "";
		if (number == null)
			return str;
		NumberFormat fmt = NumberFormat.getFormat(FORMAT_PATTERN_BYTES);
		str = fmt.format(number);
		str = str.replace(',', I18N.groupingSepator());
		return str;
	}

	public static String formatDouble(Double number) {
		String str = "";
		if (number == null)
			return str;
		NumberFormat fmt = NumberFormat.getFormat("#,###.##");
		str = fmt.format(number);
		str = str.replace(',', '#');
		str = str.replace('.', I18N.decimalSepator());
		str = str.replace('#', I18N.groupingSepator());
		return str;
	}

	public static String formatSizeKB(Object value) {
		if (value == null)
			return null;
		if (value instanceof Double doubleVal)
			return Util.formatSizeKB(doubleVal.doubleValue());
		if (value instanceof Long longVal)
			return Util.formatSizeKB(longVal.doubleValue());
		else if (value instanceof Integer intVal)
			return Util.formatSizeKB(intVal.doubleValue());
		if (value instanceof String str)
			return Util.formatSizeKB(Long.parseLong(str));
		else
			return Util.formatSizeKB(0L);
	}

	/**
	 * Format file size in KB.
	 * 
	 * @param size The file size in bytes.
	 * 
	 * @return The formated file size.
	 */
	public static String formatSizeKB(double size) {
		String str;
		if (size < 1) {
			str = "0 KB";
		} else if (size < 1024) {
			str = "1 KB";
		} else {
			NumberFormat fmt = NumberFormat.getFormat(FORMAT_PATTERN_BYTES);
			str = fmt.format(Math.ceil(size / 1024)) + " KB";
			str = str.replace(',', I18N.groupingSepator());
		}
		return str;
	}

	/**
	 * Format file size in Windows 7 Style.
	 * 
	 * @param value The file size in bytes(can be Float, Long, Integer or
	 *        String)
	 * 
	 * @return The formated file size
	 */
	public static String formatSizeW7(Object value) {
		if (value == null)
			return null;
		if (value instanceof Float floatVal)
			return Util.formatSizeKB(floatVal.doubleValue());
		if (value instanceof Long longVal)
			return Util.formatSizeW7(longVal.doubleValue());
		else if (value instanceof Integer intVal)
			return Util.formatSizeW7(intVal.doubleValue());
		if (value instanceof String str)
			return Util.formatSizeW7(Long.parseLong(str));
		else
			return Util.formatSizeW7(0L);
	}

	/**
	 * Format file size in Windows 7 Style.
	 * 
	 * @param size The file size in bytes.
	 * 
	 * @return The formated file size.
	 */
	public static String formatSizeW7(double size) {
		if (size < 0)
			return "";

		double kb = 1024;
		double mb = 1024 * kb;
		double gb = 1024 * mb;
		double tb = 1024 * gb;

		String str;
		if (size < 1) {
			str = "0 bytes";
		} else if (size < kb) {
			str = size + " bytes";
		} else if (size < mb) {
			double tmp = size / kb;
			str = formatSizeW7(tmp, "KB");
		} else if (size < gb) {
			double tmp = size / mb;
			str = formatSizeW7(tmp, "MB");
		} else if (size < tb) {
			double tmp = size / gb;
			str = formatSizeW7(tmp, "GB");
		} else {
			double tmp = size / tb;
			str = formatSizeW7(tmp, "TB");
		}
		return str;
	}

	private static String formatSizeW7(double size, String unitSymbol) {
		String str;
		if (size < 10) {
			NumberFormat fmt = NumberFormat.getFormat(FORMAT_PATTERN_TWO_DIGITS);
			str = fmt.format(size) + " " + unitSymbol;
		} else if (size < 100) {
			NumberFormat fmt = NumberFormat.getFormat(FORMAT_PATTERN_ONE_DIGIT);
			str = fmt.format(size) + " " + unitSymbol;
		} else {
			NumberFormat fmt = NumberFormat.getFormat("###");
			str = fmt.format(size) + " " + unitSymbol;
		}
		str = str.replace('.', I18N.decimalSepator());
		return str;
	}

	/**
	 * Format file size in bytes
	 * 
	 * @param size The file size in bytes
	 * 
	 * @return the formatted size
	 */
	public static String formatSizeBytes(double size) {
		String str;
		NumberFormat fmt = NumberFormat.getFormat(FORMAT_PATTERN_BYTES);
		str = fmt.format(size) + " bytes";
		str = str.replace(',', I18N.groupingSepator());
		return str;
	}

	/**
	 * Format number percentage.
	 * 
	 * @param value The value to be formatted.
	 * @param fixed The number of decimal places.
	 * @return The formated value.
	 */
	public static native String formatPercentage(double value, int fixed) /*-{
		str = value.toFixed(fixed);
		return str + "%";
	}-*/;

	/**
	 * Get browser language
	 * 
	 * @return The language in ISO 639 format.
	 */
	public static native String getBrowserLanguage() /*-{
		var lang = window.navigator.language ? window.navigator.language
				: window.navigator.userLanguage;
		if (lang != null && lang != "") {
			return lang.replace('-', '_');
		} else {
			return "en";
		}
	}-*/;

	/**
	 * Detects the user agent(browser's family)
	 *
	 * @return 'opera', 'safari', 'ie6', 'ie7', 'gecko', or 'unknown'
	 */
	public static native String getUserAgent() /*-{
		try {
			if (window.opera)
				return 'opera';
			var ua = navigator.userAgent.toLowerCase();
			if (ua.indexOf('webkit') != -1)
				return 'safari';
			if (ua.indexOf('msie 6.0') != -1)
				return 'ie6';
			if (ua.indexOf('msie 7.0') != -1)
				return 'ie7';
			if (ua.indexOf('gecko') != -1)
				return 'gecko';
			return 'unknown';
		} catch (e) {
			return 'unknown'
		}
	}-*/;

	public static native boolean isValidEmail(String email) /*-{
		var reg1 = /(@.*@)|(\.\.)|(@\.)|(\.@)|(^\.)/; // not valid
		var reg2 = /^.+\@(\[?)[a-zA-Z0-9\-\.]+\.([a-zA-Z]{2,3}|[0-9]{1,3})(\]?)$/; // valid
		return !reg1.test(email) && reg2.test(email);
	}-*/;

	public static native void redirect(String url)
	/*-{
	    $wnd.location.href = url;
	}-*/;

	public static String padLeft(String s, int n) {
		if (s.length() > n) {
			return s.substring(0, n - 3) + "...";
		} else
			return s;
	}

	/**
	 * Removes the javascript function used to ask confirmation on window close
	 */
	public static native void uninstallCloseWindowAlert() /*-{
		$wnd.onbeforeunload = null;
	}-*/;

	/**
	 * Declares the javascript function used to ask confirmation on window close
	 */
	public static native void installCloseWindowAlert() /*-{
		$wnd.onbeforeunload = function() {
			return "You have attempted to leave this page. Are you sure?";
		};
	}-*/;

	private static boolean isWebstartMode() {
		return WindowUtils.isChrome()
				|| (WindowUtils.isWindows() && "webstart".equals(Session.get().getConfig("gui.webstart.mode")));
	}

	public static void openEditWithOffice(long docId) {
		uninstallCloseWindowAlert();
		try {
			WindowUtils.openUrl("ldedit:" + GWT.getHostPageBaseURL() + "ldedit?action=edit&sid="
					+ Session.get().getSid() + "&docId=" + docId);
			ToastNotification.showNotification(I18N.message("officeaddinhintlauncher"));
		} finally {
			installCloseWindowAlert();
		}
	}

	public static void openScan() {
		if (!WindowUtils.isWindows())
			return;
		Map<String, String> params = new HashMap<>();
		params.put("targetFolderId", "" + FolderController.get().getCurrentFolder().getId());

		String url = Util.webstartURL("scan", params);

		openWebstartApp(url);
	}

	public static void openBulkCheckout(List<Long> unlockedIds) {
		Map<String, String> params = new HashMap<>();
		params.put("targetFolderId", "" + FolderController.get().getCurrentFolder().getId());
		params.put("docIds", unlockedIds.toString().replace('[', ' ').replace(']', ' ').trim());
		String url = Util.webstartURL("bulkcheckout", params);

		openWebstartApp(url);
	}

	private static void openWebstartApp(String url) {
		uninstallCloseWindowAlert();

		try {
			if (isWebstartMode()) {
				url = url.replace("=", "_x_");
				url = url.replace("&", "_y_");
				url = url.replace(",", "_z_");
				WindowUtils.openUrl("ldwebstart:" + url, "_self");
				EventPanel.get().info(I18N.message("webstarthintlauncher"), null);
			} else {
				WindowUtils.openUrl(url, "_self");
				EventPanel.get().info(I18N.message("webstarthint"), null);
			}
		} finally {
			installCloseWindowAlert();
		}
	}

	/**
	 * Checks if the passed filename can be uploaded or not on the basis of what
	 * configured in 'upload.disallow'
	 * 
	 * @param filename the file name
	 * 
	 * @return if the extension is allowed
	 */
	public static boolean isAllowedForUpload(String filename) {
		Session session = Session.get();
		if (session == null)
			return true;
		String disallow = session.getConfig("upload.disallow");
		if (disallow == null || disallow.trim().isEmpty())
			return true;

		// Extract and normalize the extensions
		String[] disallowedExtensions = disallow.split(",");
		for (int i = 0; i < disallowedExtensions.length; i++) {
			disallowedExtensions[i] = disallowedExtensions[i].toLowerCase().trim();
			if (!disallowedExtensions[i].startsWith("."))
				disallowedExtensions[i] = "." + disallowedExtensions[i];
		}

		for (int i = 0; i < disallowedExtensions.length; i++)
			if (filename.toLowerCase().endsWith(disallowedExtensions[i]))
				return false;

		return true;
	}

	/**
	 * Detect tenant specification from the request
	 * 
	 * @return the current tenant's name
	 */
	public static String detectTenant() {
		String tenant = getTenantInRequest();
		if (tenant == null)
			tenant = Constants.TENANT_DEFAULTNAME;
		return tenant;
	}

	public static String getTenantInRequest() {
		RequestInfo request = WindowUtils.getRequestInfo();
		String tenant = null;
		if (request.getParameter(Constants.TENANT) != null && !request.getParameter(Constants.TENANT).equals("")) {
			tenant = request.getParameter(Constants.TENANT);
		}
		return tenant;
	}

	public static String getLocaleInRequest() {
		RequestInfo request = WindowUtils.getRequestInfo();
		String locale = null;
		if (request.getParameter(Constants.LOCALE) != null && !request.getParameter(Constants.LOCALE).equals("")) {
			locale = request.getParameter(Constants.LOCALE);
		}
		return locale;
	}

	/**
	 * Detect locale specification from the request
	 * 
	 * @return the locale specification
	 */
	public static String detectLocale() {
		String locale = Util.getLocaleInRequest();

		// Tries to capture locale parameter
		if (locale == null)
			locale = Util.getBrowserLanguage();

		if (locale == null || locale.isEmpty())
			return "en";

		return locale;
	}

	/**
	 * Detect KEY specification from the request
	 * 
	 * @return the secret KEY
	 */
	public static String detectKey() {
		String key = null;

		try {
			RequestInfo request = WindowUtils.getRequestInfo();
			key = request.getParameter(Constants.KEY);
		} catch (Exception t) {
			// Nothing to do
		}

		return key;
	}

	public static String getParameter(String name) {
		String val = null;

		try {
			RequestInfo request = WindowUtils.getRequestInfo();
			val = request.getParameter(name);
		} catch (Exception t) {
			// Nothing to do
		}

		return val;
	}

	public static Map<String, String> getParameters() {
		Map<String, String> val = new HashMap<>();

		try {
			RequestInfo request = WindowUtils.getRequestInfo();
			val = request.getParameterMap();
		} catch (Exception t) {
			// Nothing to do
		}

		return val;
	}

	/**
	 * Detect skin specification from the request
	 * 
	 * @return the current skin's name
	 */
	private static String detectSkin() {
		String skin = null;

		try {
			RequestInfo request = WindowUtils.getRequestInfo();
			skin = request.getParameter(Constants.SKIN);
		} catch (Exception t) {
			// Nothing to do
		}

		return skin;
	}

	public static void redirectToRoot() {
		Util.redirectToRoot(null, null);
	}

	public static void redirectToRoot(String moduleName) {
		Util.redirectToRoot(moduleName, null);
	}

	private static void redirectToRoot(String moduleName, String parameters) {
		String base = GWT.getHostPageBaseURL();
		String module = GWT.getModuleName();
		if (moduleName != null)
			module = moduleName;
		String url = base + (base.endsWith("/") ? module + ".jsp" : "/" + module + ".jsp");
		url += "?locale=" + I18N.getLocale() + "&tenant=" + Session.get().getTenantName();
		if (parameters != null)
			url += "&" + parameters;
		Util.download(url);
	}

	/**
	 * Redirects to the configured page after a successful login (the url
	 * specified in the j_successurl javascript variable
	 * 
	 * @param locale language specification
	 */
	public static void redirectToSuccessUrl(String locale) {
		String url = Util.getJavascriptVariable("j_successurl");
		if (locale != null && !"".equals(locale)) {
			if (url.contains("?")) {
				url += AND_LOCALE_EQUAL + locale;
			} else {
				url += "?locale=" + locale;
			}
		}
		WindowUtils.openUrl(url);
	}

	/**
	 * Redirects to the configured login page (the url specified in the
	 * j_loginurl javascript variable
	 * 
	 * @param tenant name of the tenant
	 */
	public static void redirectToLoginUrl(String tenant) {
		String url = getLoginUrl(tenant);
		Util.redirect(url);
	}

	public static String getLoginUrl(String tenant) {
		String url = Util.getJavascriptVariable("j_loginurl");
		url += "?tenant=" + tenant;
		url += AND_LOCALE_EQUAL + I18N.getLocale();
		return url;
	}

	public static String getValue(String name, List<GUIParameter> parameters) {
		return parameters.stream().filter(param -> name.equals(param.getName())).map(GUIParameter::getValue).findFirst()
				.orElse(null);
	}

	public static long[] toPrimitives(Long[] objects) {
		long[] primitives = new long[objects.length];
		for (int i = 0; i < objects.length; i++)
			primitives[i] = objects[i];

		return primitives;
	}

	public static String getBaseName(String fileName) {
		if (fileName.indexOf('.') < 0)
			return fileName;
		return fileName.substring(0, fileName.lastIndexOf('.'));
	}

	public static String getExtension(String fileName) {
		if (fileName.indexOf('.') < 0)
			return "";
		return fileName.substring(fileName.lastIndexOf('.') + 1, fileName.length());
	}

	public static void setupDensity(GUIInfo info) {
		String density = info.getConfig("gui.density");
		if (CookiesManager.get(CookiesManager.COOKIE_DENSITY) != null)
			density = CookiesManager.get(CookiesManager.COOKIE_DENSITY);

		if ("dense".equals(density)) {
			Canvas.resizeFonts(0);
			Canvas.resizeControls(0);
		} else if ("compact".equals(density)) {
			Canvas.resizeFonts(1);
			Canvas.resizeControls(2);
		} else if ("medium".equals(density)) {
			Canvas.resizeFonts(2);
			Canvas.resizeControls(4);
		} else if ("expanded".equals(density)) {
			Canvas.resizeFonts(2);
			Canvas.resizeControls(6);
		} else if ("spacious".equals(density)) {
			Canvas.resizeFonts(3);
			Canvas.resizeControls(10);
		}
	}

	public static Map<String, String> convertToMap(List<GUIParameter> parameters) {
		Map<String, String> map = new HashMap<>();
		for (GUIParameter param : parameters)
			map.put(param.getName(), param.getValue());
		return map;
	}

	public static void waitForUpAndRunning(String tenant, String locale) {
		final String url = Util.getJavascriptVariable("j_loginurl") + "?tenant=" + tenant + AND_LOCALE_EQUAL + locale;

		LD.contactingServer();

		RequestBuilder checkRequest = new RequestBuilder(RequestBuilder.HEAD, url);
		checkRequest.setCallback(new RequestCallback() {
			@Override
			public void onResponseReceived(Request request, Response response) {
				if (response.getStatusCode() > 199 && response.getStatusCode() < 300) {
					Timer timer = new Timer() {
						public void run() {
							LD.clearPrompt();
							CookiesManager.removeSid();
							Util.uninstallCloseWindowAlert();
							ApplicationRestarting.get(I18N.message("applicationisupagainpleaseclose")).show();
						}
					};
					timer.schedule(5000);
				} else {
					Timer timer = new Timer() {
						public void run() {
							waitForUpAndRunning(tenant, locale);
						}
					};
					timer.schedule(5000);
				}
			}

			@Override
			public void onError(Request request, Throwable exception) {
				Timer timer = new Timer() {
					public void run() {
						waitForUpAndRunning(tenant, locale);
					}
				};
				timer.schedule(5000);
			}
		});
		try {
			checkRequest.send();
		} catch (RequestException e) {
			// Nothing to do
		}
	}

	/**
	 * Converts some HTML specific chars into it's entity
	 * 
	 * @param originalText the original string to filter
	 * 
	 * @return the escaped string
	 */
	public static String escapeHTML(String originalText) {
		if (originalText == null)
			return "";
		return originalText.replace("<", "&lt;").replace(">", "&gt;");
	}

	/**
	 * Manipulates the given Base64 image specification into a Src to be uses in
	 * a &lt;img&gt; tag
	 * 
	 * @param imageBase64 the Base64 representation of an image
	 * 
	 * @return The Src
	 */
	public static String imageBase64Src(String imageBase64) {
		if (imageBase64 != null && !imageBase64.isEmpty() && !imageBase64.startsWith("data:image"))
			return "data:image/png;base64," + imageBase64;
		else
			return imageBase64;
	}

	/**
	 * Formats a set of tags into an HTML grid
	 * 
	 * @param tags the tags to format
	 * 
	 * @return the HTML content
	 */
	public static String getTagsHTML(List<String> tags) {
		StringBuilder buf = new StringBuilder(
				"<div style='display: grid; grid-gap: 2px; padding: 1px; grid-auto-flow: row dense; grid-template-columns: auto auto auto; grid-template-rows: auto auto;'>");
		for (String tag : tags) {
			buf.append("<span class='button' style='white-space: nowrap;'>");
			buf.append(tag);
			buf.append("</span>");
		}
		buf.append(END_DIV);
		return buf.toString();
	}

	public static String formatDateShortISO(Date date) {
		DateTimeFormat fmt = DateTimeFormat.getFormat("yyyy-MM-dd");
		return fmt.format(date);
	}

	public static String encodeUTF8(String rawString) {
		try {
			byte[] bytes = rawString.getBytes(StandardCharsets.UTF_8);
			return new String(bytes, StandardCharsets.UTF_8);
		} catch (Exception t) {
			return rawString;
		}
	}

	public static GUIParameter getParameter(List<GUIParameter> params, String name) {
		try {
			return params.stream().filter(param -> param.getName().equals(Session.get().getTenantName() + "." + name)
					|| param.getName().equals(name)).findFirst().orElse(null);
		} catch (RuntimeException re) {
			return null;
		}
	}

	public static String getParameterValue(List<GUIParameter> params, String name) {
		try {
			GUIParameter param = getParameter(params, name);
			return param != null ? param.getValue() : null;
		} catch (RuntimeException re) {
			return null;
		}
	}

	public static Boolean getParameterValueAsBoolean(List<GUIParameter> params, String name) {
		try {
			GUIParameter param = getParameter(params, name);
			return param != null ? param.getValueAsBoolean() : null;
		} catch (RuntimeException re) {
			return Boolean.FALSE;
		}
	}

	public static void removeChildren(Layout container) {
		Canvas[] members = container.getMembers();
		if (members != null)
			for (Canvas member : members) {
				container.removeChild(member);
			}
	}

	public static native String getJavascriptVariable(String jsVar)/*-{
		return eval('$wnd.' + jsVar);
	}-*/;
}