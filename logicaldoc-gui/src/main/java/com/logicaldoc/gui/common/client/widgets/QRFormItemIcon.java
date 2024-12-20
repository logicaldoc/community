package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.Feature;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;

/**
 * An icon to be used display a QR Code
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.1.1
 */
public class QRFormItemIcon extends FormItemIcon {

	/**
	 * The constructor.
	 * 
	 * @param code the text to render in the QR Code
	 * @param tooltip the tooltip to display
	 */
	public QRFormItemIcon(String code, String tooltip) {
		setName("qrcode");
		if (Feature.isCommercial()) {
			setPrompt(I18N.message(tooltip));
			setSrc("[SKIN]/qrcode.svg");
			setWidth(16);
			setHeight(16);

			addFormItemClickHandler(event -> {
				String content = code;
				if (content == null)
					content = event.getItem().getValue().toString();
				new QRLightbox(content, "qrcode", 150).show();
			});
		} else {
			setSrc("[SKIN]/blank.png");
			setText("");
			setWidth(16);
			setHeight(16);
			setDisabled(true);
		}
	}

	/**
	 * The constructor.
	 * 
	 * @param code the text to render in the QR Code
	 */
	public QRFormItemIcon(String code) {
		this(code, "qrcode");
	}

	/**
	 * The constructor, the current item's text will be user for the QR Code
	 */
	public QRFormItemIcon() {
		this(null);
	}
}
