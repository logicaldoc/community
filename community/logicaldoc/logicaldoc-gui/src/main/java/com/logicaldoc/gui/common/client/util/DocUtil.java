package com.logicaldoc.gui.common.client.util;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
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
			return "blank.png";

		if (indexed.intValue() == Constants.INDEX_INDEXED) {
			return "indexed.png";
		} else if (indexed.intValue() == Constants.INDEX_SKIP) {
			return "unindexable.png";
		} else
			return "blank.png";
	}

	public static String getLockedIcon(Integer locked) {
		if (locked == null)
			return "blank.png";

		if (locked.intValue() == Constants.DOC_LOCKED) {
			return "lock.png";
		} else if (locked.intValue() == Constants.DOC_CHECKED_OUT) {
			return "page_edit.png";
		} else
			return "blank.png";
	}

	public static String getImmutableIcon(Integer immutable) {
		if (immutable == null)
			return "blank.png";

		if (immutable.intValue() == 1)
			return "stop.png";

		return "blank.png";
	}

	public static String getPasswordProtectedIcon(Boolean password) {
		if (password == null)
			return "blank.png";

		if (password.booleanValue())
			return "key.png";

		return "blank.png";
	}

	public static String getSignedIcon(Integer signed) {
		if (signed == null)
			return "blank.png";

		if (signed.intValue() == 1)
			return "rosette.png";

		return "blank.png";
	}

	public static String getStampedIcon(Integer stamped) {
		if (stamped == null)
			return "blank.png";

		if (stamped.intValue() == 1)
			return "stamp.png";

		return "blank.png";
	}

	public static String getBookmarkedIcon(boolean bookmarked) {
		if (bookmarked)
			return "heart.png";
		else
			return "blank.png";
	}
}