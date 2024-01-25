package com.logicaldoc.gui.frontend.client.metadata.zonalocr;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.beans.GUIAttribute;
import com.logicaldoc.gui.common.client.beans.GUIZone;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.widgets.ImageWithCanvases;
import com.logicaldoc.gui.frontend.client.metadata.template.AttributeTypeFormatter;
import com.logicaldoc.gui.frontend.client.services.DocumentService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
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

	private ValuesManager vm;

	private DynamicForm form;

	private GUIZone zone;

	public ZoneEditor(GUIZone zone) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("zone") + " - " + zone.getName());
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
		DocumentService.Instance.get().cleanUploadedFileFolder(new AsyncCallback<Void>() {

			@Override
			public void onFailure(Throwable caught) {
				// Nothing to do
			}

			@Override
			public void onSuccess(Void result) {
				// Nothing to do
			}
		});
	}

	private void prepareForm() {
		form = new DynamicForm();
		form.setWidth100();
		form.setAlign(Alignment.LEFT);
		form.setColWidths("1px, 100%");
		vm = new ValuesManager();
		form.setValuesManager(vm);

		StaticTextItem type = ItemFactory.newStaticTextItem("type", AttributeTypeFormatter.format(zone.getType()));

		StaticTextItem sample = ItemFactory.newStaticTextItem("sample", zone.getSampleText());
		sample.setVisible(zone.getSampleText() != null && !zone.getSampleText().isEmpty());

		TextItem format = ItemFactory.newTextItem("format", zone.getFormat());
		format.setVisible(zone.getType() != GUIAttribute.TYPE_BOOLEAN && zone.getType() != GUIAttribute.TYPE_STRING);

		TextItem decimalSeparator = ItemFactory.newTextItem("decimalseparator", zone.getDecimalSeparator());
		decimalSeparator.setLength(1);
		decimalSeparator.setWidth(50);
		decimalSeparator.setVisible(zone.getType() == GUIAttribute.TYPE_DOUBLE);

		TextItem groupingSeparator = ItemFactory.newTextItem("groupingseparator", zone.getGroupingSeparator());
		groupingSeparator.setLength(1);
		groupingSeparator.setWidth(50);
		groupingSeparator.setVisible(zone.getType() == GUIAttribute.TYPE_INT
				|| zone.getType() == GUIAttribute.TYPE_DOUBLE || zone.getType() == GUIAttribute.TYPE_FOLDER
				|| zone.getType() == GUIAttribute.TYPE_USER || zone.getType() == GUIAttribute.TYPE_DOCUMENT);

		SelectItem language = ItemFactory.newLanguageSelector("language", true, false);
		language.setValue(zone.getLanguage());

		TextAreaItem parsing = ItemFactory.newTextAreaItemForAutomation("parsing", zone.getParsing(), null, false);
		parsing.setHeight(100);
		parsing.setWidth(300);

		form.setItems(type, sample, language, format, decimalSeparator, groupingSeparator, parsing);
	}

	public void onSave() {
		if (Boolean.FALSE.equals(vm.validate()))
			return;

		zone.setFormat(vm.getValueAsString("format"));
		zone.setDecimalSeparator(vm.getValueAsString("decimalseparator"));
		zone.setGroupingSeparator(vm.getValueAsString("groupingseparator"));
		zone.setParsing(vm.getValueAsString("parsing"));
		zone.setLanguage(vm.getValueAsString("language"));

		destroy();
	}
}