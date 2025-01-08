package com.logicaldoc.gui.frontend.client.metadata.barcode;

import com.logicaldoc.gui.common.client.IgnoreAsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeZone;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ImageWithCanvases;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.MultiComboBoxItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to show/edit the Zone settings
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class ZoneEditor extends Window {

	private DynamicForm form;

	private GUIBarcodeZone zone;

	public ZoneEditor(GUIBarcodeZone zone) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("zone"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		this.zone = zone;

		IButton save = new IButton(I18N.message("save"));
		save.addClickHandler(event -> onSave());

		prepareForm();

		ImageWithCanvases sample = new ImageWithCanvases(zone.getSampleUrl(), 300, null, null);

		HLayout body = new HLayout();
		body.setMembers(form, sample);

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setWidth100();
		layout.setMembers(body, save);

		addItem(layout);

		// Just to clean the upload folder
		DocumentService.Instance.get().cleanUploadedFileFolder(new IgnoreAsyncCallback<>());
	}

	private void prepareForm() {
		form = new DynamicForm();
		form.setWidth100();
		form.setAlign(Alignment.LEFT);
		form.setColWidths("1px, 100%");

		StaticTextItem sample = ItemFactory.newStaticTextItem("sample", zone.getSampleText());
		sample.setVisible(zone.getSampleText() != null && !zone.getSampleText().isEmpty());

		TextItem patterns = ItemFactory.newTextItem("patterns", zone.getPatterns());
		patterns.setWidth(300);
		patterns.setRequired(true);

		TextItem include = ItemFactory.newTextItem("include", zone.getInclude());
		include.setWidth(200);

		TextItem exclude = ItemFactory.newTextItem("exclude", zone.getExclude());
		exclude.setWidth(200);

		MultiComboBoxItem formats = ItemFactory.newBarcodeFormatsComboBoxItem(zone.getFormats());

		form.setItems(sample, patterns, include, exclude, formats);
	}

	public void onSave() {
		if (!form.validate())
			return;

		zone.setPatterns(form.getValueAsString("patterns"));
		zone.setInclude(form.getValueAsString("include"));
		zone.setExclude(form.getValueAsString("exclude"));
		zone.setFormats(form.getValueAsString("formats"));

		destroy();
	}
	
	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}