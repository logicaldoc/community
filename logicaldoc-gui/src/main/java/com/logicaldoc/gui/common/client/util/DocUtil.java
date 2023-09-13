package com.logicaldoc.gui.common.client.util;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.controllers.DocumentController;
import com.logicaldoc.gui.common.client.controllers.UserController;

/**
 * Some utility methods for the documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6
 */
public class DocUtil {

	private DocUtil() {
	}

	public static void download(final long docId, final String fileVersion) {
		download(docId, fileVersion, null);
	}

	public static void download(final long docId, final String fileVersion, final String suffix) {
		DocumentProtectionManager.askForPassword(docId, document -> {
			if (suffix != null)
				Util.download(Util.downloadURL(docId, fileVersion, false) + suffix);
			else
				Util.download(Util.downloadURL(docId, fileVersion, false));
		});
	}

	public static void downloadPdfConversion(final long docId, final String fileVersion) {
		DocumentProtectionManager.askForPassword(docId, document -> {
			if (fileVersion != null)
				Util.download(Util.contextPath() + "convertpdf?docId=" + docId + "&version=" + fileVersion);
			else
				Util.download(Util.contextPath() + "convertpdf?docId=" + docId);
		});
	}

	public static void markCheckedOut(GUIDocument doc) {
		doc.setStatus(Constants.DOC_CHECKED_OUT);
		doc.setLockUserId(Session.get().getUser().getId());
		doc.setLockUser(Session.get().getUser().getFullName());
		DocumentController.get().checkedOut(doc);

		int count = Session.get().getUser().getCheckedOutDocs() + 1;
		Session.get().getUser().setCheckedOutDocs(count);
		UserController.get().changed(Session.get().getUser());
	}

	public static void markCheckedIn(GUIDocument doc) {
		doc.setStatus(Constants.DOC_UNLOCKED);
		doc.setLockUserId(null);
		doc.setLockUser(null);
		DocumentController.get().checkedIn(doc);

		int count = Session.get().getUser().getCheckedOutDocs() - 1;
		Session.get().getUser().setCheckedOutDocs(count < 0 ? 0 : count);
		UserController.get().changed(Session.get().getUser());
	}

	public static void markLocked(GUIDocument doc) {
		doc.setStatus(Constants.DOC_LOCKED);
		doc.setLockUserId(Session.get().getUser().getId());
		doc.setLockUser(Session.get().getUser().getFullName());
		DocumentController.get().locked(doc);

		int count = Session.get().getUser().getLockedDocs() + 1;
		Session.get().getUser().setLockedDocs(count);
		UserController.get().changed(Session.get().getUser());
	}

	public static void markUnlocked(GUIDocument doc) {
		doc.setStatus(Constants.DOC_UNLOCKED);
		doc.setLockUserId(null);
		doc.setLockUser(null);
		DocumentController.get().unlocked(doc);

		int count = Session.get().getUser().getLockedDocs() - 1;
		Session.get().getUser().setLockedDocs(count < 0 ? 0 : count);
		UserController.get().changed(Session.get().getUser());
	}

	public static String getLockedIcon(Integer locked) {
		if (locked == null)
			return "";

		if (locked.intValue() == Constants.DOC_LOCKED) {
			return AwesomeFactory.getIconHtml("lock-alt");
		} else if (locked.intValue() == Constants.DOC_CHECKED_OUT) {
			return AwesomeFactory.getIconHtml("edit");
		} else
			return "";
	}

	public static String getImmutableIcon(Integer immutable) {
		if (immutable == null)
			return "";

		if (immutable.intValue() == 1)
			return AwesomeFactory.getIconHtml("hand-paper");

		return "";
	}

	public static String getPasswordProtectedIcon(Boolean password) {
		if (password == null)
			return "";

		if (password.booleanValue())
			return AwesomeFactory.getIconHtml("key");

		return "";
	}

	public static String getSignedIcon(Integer signed) {
		if (signed == null)
			return "";

		if (signed.intValue() == 1)
			return AwesomeFactory.getIconHtml("badge-check");

		return "";
	}

	public static String getStampedIcon(Integer stamped) {
		if (stamped == null)
			return "";

		if (stamped.intValue() == 1)
			return AwesomeFactory.getIconHtml("tint");

		return "";
	}

	public static String getBookmarkedIcon(boolean bookmarked) {
		if (bookmarked)
			return AwesomeFactory.getIconHtml("bookmark");
		else
			return "";
	}

	public static String getRatingIcon(Integer rating) {
		StringBuilder html = new StringBuilder();
		if (rating != null)
			for (int i = 1; i <= rating; i++) {
				html.append(AwesomeFactory.getIconHtml("star"));
			}
		return html.toString();
	}

	public static String getFolderIcon(boolean open, int type, String name, String color) {
		String html = "";

		if (type == GUIFolder.TYPE_WORKSPACE)
			html = AwesomeFactory.getColoredIconHtml("cube", name, color);
		else if (type == GUIFolder.TYPE_ALIAS) {
			html = "<div><span class='fa-layers fa-fw'>";
			html += "<i class='" + AwesomeFactory.getCssClassPrefix() + " fa-lg fa-fw fa-"
					+ (open ? "folder-open" : "folder") + " ' "
					+ (color != null && !color.isEmpty() ? "style='color: " + color + "'" : "") + "></i>";
			// This is the small alias arrow
			html += "<i class='fas fa-lg fa-fw fa-share' data-fa-transform='shrink-10 down-2 right-2'></i></span>&nbsp;&nbsp;&nbsp;";
			html += name + "</div>";
		} else {
			if (open)
				html = AwesomeFactory.getColoredIconHtml("folder-open", name, color);
			else
				html = AwesomeFactory.getColoredIconHtml("folder", name, color);
		}

		return html;
	}
}