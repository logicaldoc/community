package com.logicaldoc.gui.common.client.util;

import java.util.HashMap;
import java.util.Map;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIDocument;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.util.SC;

/**
 * Utility class for handling passwords on documents.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6
 */
public class DocumentProtectionManager {

	// Stores the doc's ID and the password used to un-protect it
	private static Map<Long, String> unprotectedDocs = new HashMap<>();

	private DocumentProtectionManager() {
	}

	public static boolean isUnprotected(long docId) {
		return unprotectedDocs.containsKey(docId);
	}

	/**
	 * Checks the document and asks the user for a password(if needed).
	 * 
	 * @param document the document to check
	 * @param handler Optional handler to react something when the document gets
	 *        unlocked
	 */
	public static void askForPassword(long docId, final DocumentProtectionHandler handler) {
		DocumentService.Instance.get().getById(docId, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(GUIDocument document) {
				askForPassword(document, handler);
			}
		});
	}

	/**
	 * Checks the document and asks the user for a password(if needed).
	 * 
	 * @param document the document to check
	 * @param handler Optional handler to react something when the document gets
	 *        unlocked
	 */
	public static void askForPassword(GUIDocument document, final DocumentProtectionHandler handler) {
		DocumentService.Instance.get().isPasswordProtected(document.getId(), new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Boolean passwordProtected) {
				if (Boolean.TRUE.equals(passwordProtected))
					handlePasswordProtectedDocument(document.getId(), handler);
				else
					notifyUnprotected(handler, document);
			}
		});
	}

	/**
	 * This method gets invoked when a document with password has been hit
	 * 
	 * @param docId identifier of the document
	 * @param handler Optional handler to react something when the document gets
	 *        unlocked
	 */
	private static void handlePasswordProtectedDocument(final long docId, DocumentProtectionHandler handler) {
		DocumentService.Instance.get().getById(docId, new DefaultAsyncCallback<>() {

			@Override
			public void onSuccess(final GUIDocument result) {
				if (unprotectedDocs.containsKey(docId)) {
					notifyUnprotected(handler, result);
				} else {
					LD.askForDocumentPassword(I18N.message("security"), I18N.message("enterprotpassword"), null,
							(final String password) -> {
								if (password == null) {
									SC.warn(I18N.message("accessdenied"));
									return;
								}

								if ("--unset--".equals(password)
										&& Session.get().getUser().isMemberOf(Constants.GROUP_ADMIN)) {
									unsetPassword(docId, handler, result, password);
								} else {
									unprotect(docId, handler, result, password);
								}
							});
				}
			}
		});
	}

	private static void unsetPassword(final Long docId, final DocumentProtectionHandler handler,
			final GUIDocument result, final String password) {
		DocumentService.Instance.get().unsetPassword(result.getId(), password, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Void res) {
				saveProtectionPasswordAndNotify(docId, handler, result, password);
			}
		});
	}

	private static void unprotect(final Long docId, final DocumentProtectionHandler handler, final GUIDocument result,
			final String password) {
		DocumentService.Instance.get().unprotect(result.getId(), password, new DefaultAsyncCallback<>() {
			@Override
			public void onSuccess(Boolean granted) {
				if (Boolean.TRUE.equals(granted)) {
					saveProtectionPasswordAndNotify(docId, handler, result, password);
				} else if (handler != null) {
					SC.warn(I18N.message("accessdenied"));
				}
			}
		});
	}

	private static void notifyUnprotected(final DocumentProtectionHandler handler, final GUIDocument document) {
		if (handler != null)
			handler.onUnprotected(document);
	}

	private static void saveProtectionPasswordAndNotify(final Long docId, final DocumentProtectionHandler handler,
			final GUIDocument document, final String password) {
		// Save the password for further reference
		unprotectedDocs.put(docId, password);
		notifyUnprotected(handler, document);
	}

	public interface DocumentProtectionHandler {
		public abstract void onUnprotected(GUIDocument document);
	}
}