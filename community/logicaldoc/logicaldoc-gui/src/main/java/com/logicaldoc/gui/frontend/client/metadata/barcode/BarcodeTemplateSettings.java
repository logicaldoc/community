package com.logicaldoc.gui.frontend.client.metadata.barcode;

import com.google.gwt.user.client.rpc.AsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIBarcodeTemplate;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.log.Log;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.services.BarcodeService;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * This popup window is used to upload / edit a barcode template
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.4.2
 */
public class BarcodeTemplateSettings extends Window {

	private ValuesManager vm;

	private DynamicForm form;

	private IButton save;

	private BarcodeTemplatesPanel panel;

	private GUIBarcodeTemplate template;

	public BarcodeTemplateSettings(BarcodeTemplatesPanel panel, GUIBarcodeTemplate template) {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("barcodetemplate"));
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
		setAutoSize(true);

		this.panel = panel;
		this.template = template;

		save = new IButton(I18N.message("save"));
		save.addClickHandler(new com.smartgwt.client.widgets.events.ClickHandler() {

			@Override
			public void onClick(com.smartgwt.client.widgets.events.ClickEvent event) {
				onSave();
			}
		});

		prepareForm();

		VLayout layout = new VLayout();
		layout.setMembersMargin(5);
		layout.setWidth100();

		layout.addMember(form);
		layout.addMember(save);

		addItem(layout);
	}

	private void prepareForm() {
		form = new DynamicForm();
		form.setWidth100();
		form.setAlign(Alignment.LEFT);
		form.setColWidths("1px, 100%");
		vm = new ValuesManager();
		form.setValuesManager(vm);

		TextItem name = ItemFactory.newTextItem("name", "name", template.getName());
		name.setRequired(true);
		name.setDisabled(template.getId() != 0L);

		StaticTextItem id = ItemFactory.newStaticTextItem("id", I18N.message("id"), "" + template.getId());
		id.setVisible(template != null && template.getId() != 0L);

		TextAreaItem description = ItemFactory.newTextAreaItem("description", "description", template.getDescription());
		description.setHeight(200);

		// The optional batch
		SpinnerItem batch = ItemFactory.newSpinnerItem("batch", "batch", template.getBatch());
		batch.setRequired(true);
		batch.setMin(1);
		batch.setStep(10);
		batch.setHintStyle("hint");

		// The image threshold
		SpinnerItem resolutionThreshold = ItemFactory.newSpinnerItem("resolutionthreshold",
				I18N.message("resolutionthreshold"), template.getImageThreshold());
		resolutionThreshold.setRequired(true);
		resolutionThreshold.setWrapTitle(false);
		resolutionThreshold.setMin(50);
		resolutionThreshold.setStep(100);
		resolutionThreshold.setHint("pixels");

		if (Session.get().isDefaultTenant() && template.getId() != 0L)
			form.setItems(id, name, description, batch, resolutionThreshold);
		else
			form.setItems(id, name, description);
	}

	public void onSave() {
		if (!vm.validate())
			return;

		template.setName(vm.getValueAsString("name"));
		template.setDescription(vm.getValueAsString("description"));

		if (Session.get().isDefaultTenant() && template.getId() != 0L) {
			template.setBatch(Integer.parseInt(vm.getValueAsString("batch")));
			template.setImageThreshold(Integer.parseInt(vm.getValueAsString("resolutionthreshold")));
		}

		BarcodeService.Instance.get().save(template, new AsyncCallback<GUIBarcodeTemplate>() {

			@Override
			public void onFailure(Throwable caught) {
				Log.serverError(caught);
			}

			@Override
			public void onSuccess(GUIBarcodeTemplate tmpl) {
				panel.setSelectedBarcodeTemplate(tmpl);
				destroy();
			}
		});
	}
}