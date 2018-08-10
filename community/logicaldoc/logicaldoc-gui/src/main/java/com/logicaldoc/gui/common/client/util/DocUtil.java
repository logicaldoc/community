package com.logicaldoc.gui.common.client.util;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.beans.GUIFolder;
import com.logicaldoc.gui.common.client.observer.DocumentController;
import com.logicaldoc.gui.common.client.observer.UserController;
import com.logicaldoc.gui.common.client.util.DocumentProtectionManager.DocumentProtectionHandler;

/**
 * Some utility methods for the documents
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6
 */
public class DocUtil {

	public static void download(final long docId, final String fileVersion) {
		download(docId, fileVersion, null);
	}

	public static void download(final long docId, final String fileVersion, final String suffix) {
		DocumentProtectionManager.askForPassword(docId, new DocumentProtectionHandler() {

			@Override
			public void onUnprotected(GUIDocument document) {
				if (suffix != null)
					WindowUtils.openUrl(Util.downloadURL(docId, fileVersion, false) + suffix);
				else
					WindowUtils.openUrl(Util.downloadURL(docId, fileVersion, false));
			}
		});
	}

	public static void downloadPdfConversion(final long docId, final String fileVersion) {
		DocumentProtectionManager.askForPassword(docId, new DocumentProtectionHandler() {

			@Override
			public void onUnprotected(GUIDocument document) {
				if (fileVersion != null)
					WindowUtils.openUrl(Util.contextPath() + "convertpdf?docId=" + docId + "&version=" + fileVersion,
							"_blank");
				else
					WindowUtils.openUrl(Util.contextPath() + "convertpdf?docId=" + docId, "_blank");
			}
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

	public static String getIndexedIcon(Integer indexed) {
		if (indexed == null)
			return "";
		String html = AwesomeFactory.getIconHtml("database");
		if (indexed == Constants.INDEX_SKIP) {
			html = "<span class='fa-layers fa-fw'>" + html;
			html += "<i class='" + AwesomeFactory.getCssClassPrefix()
					+ " fa-times' data-fa-transform='grow-8' style='color: red'></i></span>";
		}
		return html;
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
		String html = "";
		if (rating != null)
			for (int i = 1; i <= rating; i++) {
				html += AwesomeFactory.getIconHtml("star");
			}
		return html;
	}

	public static String getFolderIcon(boolean open, int type, String name) {
		String html = "";

		if (type == GUIFolder.TYPE_WORKSPACE)
			html = AwesomeFactory.getIconHtml("cube", name);
		else if (type == GUIFolder.TYPE_ALIAS) {
			html = "<div><span class='fa-layers fa-fw'>";
			html += "<i class='" + AwesomeFactory.getCssClassPrefix() + " fa-lg fa-fw fa-"
					+ (open ? "folder-open" : "folder") + "'></i>";
			html += "<i class='fas fa-lg fa-fw fa-share' data-fa-transform='shrink-10 down-2 right-2'></i></span>&nbsp;&nbsp;&nbsp;";
			html += name + "</div>";
		} else {
			if (open)
				html = AwesomeFactory.getIconHtml("folder-open", name);
			else
				html = AwesomeFactory.getIconHtml("folder", name);
		}

		return html;
	}
}