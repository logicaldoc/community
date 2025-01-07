package com.logicaldoc.gui.frontend.client.document;

import java.util.Arrays;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.util.SC;

public interface DocumentUtil {

	/**
	 * Checks the DOWNLOAD permission and forwards the download of the specified
	 * document-related resource.
	 * 
	 * @param docId Identifier of thee document the URL refers to
	 * 
	 * @param url The url of a resource related to the given document,may be
	 *        null
	 */
	public static void downloadDocumentResource(String docId, String url) {
		DocumentService.Instance.get().getAllowedPermissions(Arrays.asList(Long.parseLong(docId)),
				new DefaultAsyncCallback<GUIAccessControlEntry>() {
					@Override
					public void onSuccess(GUIAccessControlEntry acl) {
						if (acl.isDownload()) {
							if (url == null || url.trim().isEmpty())
								Util.download(Util.downloadURL(Long.parseLong(docId)));
							else
								Util.download(url);
						} else {
							SC.warn(I18N.message("youdonothavedownloadpermissionondoc"));
						}
					}
				});
	}

	public static void downloadDocument(long docId) {
		downloadDocumentResource(Long.toString(docId), null);
	}
}
