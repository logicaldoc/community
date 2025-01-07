package com.logicaldoc.gui.common.client.util;

import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAccessControlEntry;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.GuiLog;
import com.logicaldoc.gui.frontend.client.services.DocumentService;

/**
 * Some utility methods related to recurrent security operations
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.0.1
 */
public class SecurityUtil {

	private SecurityUtil() {
		// Not instantiable
	}

	/**
	 * Checks if the user has all the specified permissions on the selected
	 * documents and runs the task
	 * 
	 * @param docIds Identifier of the documents to check
	 * @param requiredPermissions The permissions required on the documents
	 *        selection
	 * @param task The task to run
	 */
	public static void checkPermissionsAndRun(List<Long> docIds, String[] requiredPermissions, Runnable task) {
		if (docIds == null || docIds.isEmpty())
			return;

		DocumentService.Instance.get().getAllowedPermissions(docIds, new DefaultAsyncCallback<>() {

			@Override
			public void onSuccess(GUIAccessControlEntry grantedPermissions) {
				for (String permission : requiredPermissions) {
					if (!grantedPermissions.isPermissionAllowed(permission.toLowerCase())) {
						GuiLog.warn(I18N.message("somedocsdonothaveperm", permission.toUpperCase()), null);
						return;
					}
				}
				task.run();
			}
		});
	}
}