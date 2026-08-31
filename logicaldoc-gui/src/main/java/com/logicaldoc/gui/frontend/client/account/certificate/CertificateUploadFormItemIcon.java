package com.logicaldoc.gui.frontend.client.account.certificate;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;

/**
 * An icon to be used to trigger the certificate upload popup
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9
 */
public class CertificateUploadFormItemIcon extends FormItemIcon {

	/**
	 * The constructor.
	 * 
	 * @param title the title of the popup
	 */
	public CertificateUploadFormItemIcon(String title) {
		setPrompt(I18N.message("upload"));
		setSrc("[SKIN]/page_white_get.png");
		setWidth(16);
		setHeight(16);
		addFormItemClickHandler(event -> new CertificateUploader(title, event.getItem()).show());
	}
}
