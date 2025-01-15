package com.logicaldoc.gui.frontend.client.security.saml;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;

/**
 * An icon to be used to trigger the SAML upload popup
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9
 */
public class UploadFormItemIcon extends FormItemIcon {

	/**
	 * The constructor.
	 * 
	 * @param title the title of the popup
	 */
	public UploadFormItemIcon(String title) {
		setPrompt(I18N.message("upload"));
		setSrc("[SKIN]/upload.svg");
		setWidth(16);
		setHeight(16);
		addFormItemClickHandler(event -> new SamlUploader(title, event.getItem()).show());
	}
}
