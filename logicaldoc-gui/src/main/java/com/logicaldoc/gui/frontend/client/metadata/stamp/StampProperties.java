package com.logicaldoc.gui.frontend.client.metadata.stamp;

import java.util.Date;
import java.util.LinkedHashMap;

import com.logicaldoc.gui.common.client.beans.GUIStamp;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.types.VerticalAlignment;
import com.smartgwt.client.widgets.HTMLFlow;
import com.smartgwt.client.widgets.IButton;
import com.smartgwt.client.widgets.events.ClickEvent;
import com.smartgwt.client.widgets.events.ClickHandler;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.ValuesManager;
import com.smartgwt.client.widgets.form.fields.ColorItem;
import com.smartgwt.client.widgets.form.fields.FormItem;
import com.smartgwt.client.widgets.form.fields.IntegerItem;
import com.smartgwt.client.widgets.form.fields.RadioGroupItem;
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedEvent;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;
import com.smartgwt.client.widgets.layout.VLayout;

/**
 * Shows routine's standard properties
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.1
 */
public class StampProperties extends StampDetailsTab {

	private HLayout formsContainer = new HLayout();

	private ValuesManager vm = new ValuesManager();

	private DynamicForm form1 = null;

	private DynamicForm form2 = null;

	private VLayout image = new VLayout();

	private FormItem text;

	public StampProperties(GUIStamp stamp, final ChangedHandler changedHandler) {
		super(stamp, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(formsContainer);
		refresh();
	}

	private void refresh() {
		vm.clearValues();
		vm.clearErrors(false);

		if (formsContainer.getMembers() != null)
			formsContainer.removeMembers(formsContainer.getMembers());

		vm.clearErrors(false);
		vm.clearValues();
		vm.resetValues();

		if (form1 != null)
			form1.destroy();

		if (form2 != null)
			form2.destroy();

		form1 = new DynamicForm();
		form1.setNumCols(2);
		form1.setTitleOrientation(TitleOrientation.TOP);
		form1.setValuesManager(vm);

		form2 = new DynamicForm();
		form2.setNumCols(1);
		form2.setTitleOrientation(TitleOrientation.TOP);
		form2.setValuesManager(vm);

		if (stamp.getType() != GUIStamp.TYPE_IMAGE)
			form2.setWidth100();

		TextItem name = ItemFactory.newSimpleTextItem("name", "name", stamp.getName());
		name.addChangedHandler(changedHandler);
		name.setRequired(true);
		name.setWidth(300);
		name.setDisabled(stamp.getId() != 0L);

		TextItem exprx = ItemFactory.newTextItem("exprx", "exprx", stamp.getExprX());
		exprx.addChangedHandler(changedHandler);
		exprx.setWidth(300);

		TextItem exprw = ItemFactory.newTextItem("exprw", "exprw", stamp.getExprW());
		exprw.addChangedHandler(changedHandler);
		exprw.setWidth(300);

		TextItem expry = ItemFactory.newTextItem("expry", "expry", stamp.getExprY());
		expry.addChangedHandler(changedHandler);
		expry.setWidth(300);

		TextItem exprh = ItemFactory.newTextItem("exprh", "exprh", stamp.getExprH());
		exprh.addChangedHandler(changedHandler);
		exprh.setWidth(300);

		TextAreaItem description = ItemFactory.newTextAreaItem("description", "description", stamp.getDescription());
		description.addChangedHandler(changedHandler);
		description.setRowSpan(2);
		description.setWidth(300);

		text = ItemFactory.newTextAreaItemForAutomation("text", "text", stamp.getText(), changedHandler, false);
		text.setWidth("*");
		if (stamp.getType() == GUIStamp.TYPE_HTML) {
			text = ItemFactory.newRichTextItemForAutomation("text", "text", stamp.getText(), changedHandler);
			text.setHeight("*");
		}

		final ColorItem color = ItemFactory.newColorItemPicker("color", "color", stamp.getColor(), true,
				changedHandler);

		final RadioGroupItem barcodeLabel = ItemFactory.newBooleanSelector("barcodeLabel", "label");
		barcodeLabel.setValue(stamp.getBarcodeLabel() == 1 ? "yes" : "no");
		barcodeLabel.addChangedHandler(changedHandler);

		final SelectItem barcodeFormat = ItemFactory.newBarcodeGenerationFormatSelector("barcodeFormat", "format",
				stamp.getBarcodeFormat());
		barcodeFormat.addChangedHandler(changedHandler);

		final IntegerItem imageWidth = ItemFactory.newIntegerItem("imageWidth", "width", stamp.getImageWidth());
		imageWidth.setWidth(80);
		imageWidth.setHint("px");
		imageWidth.addChangedHandler(changedHandler);

		final IntegerItem imageHeight = ItemFactory.newIntegerItem("imageHeight", "height", stamp.getImageHeight());
		imageHeight.setWidth(80);
		imageHeight.setHint("px");
		imageHeight.addChangedHandler(changedHandler);

		final SelectItem type = ItemFactory.newSelectItem("type", "type");
		LinkedHashMap<String, String> map = new LinkedHashMap<String, String>();
		map.put("" + GUIStamp.TYPE_IMAGE, I18N.message("image"));
		map.put("" + GUIStamp.TYPE_TEXT, I18N.message("text"));
		map.put("" + GUIStamp.TYPE_HTML, I18N.message("html"));
		map.put("" + GUIStamp.TYPE_BARCODE, I18N.message("barcode"));
		type.setValueMap(map);
		type.setValue("" + stamp.getType());
		type.setWidth(80);

		SpinnerItem rotation = ItemFactory.newSpinnerItem("rotation", "rotation", stamp.getRotation(), 0, 360);
		rotation.addChangedHandler(changedHandler);

		final TextItem pageSelection = ItemFactory.newTextItem("pageSelection", I18N.message("selection"),
				stamp.getPageSelection());
		pageSelection.setVisible(stamp.getPageOption() == GUIStamp.PAGE_OPT_SEL);
		pageSelection.addChangedHandler(changedHandler);

		final RadioGroupItem pageOption = ItemFactory.newRadioGroup("pageOption", "stampin");
		LinkedHashMap<String, String> pageOptions = new LinkedHashMap<String, String>();
		pageOptions.put("" + GUIStamp.PAGE_OPT_ALL, I18N.message("allpages"));
		pageOptions.put("" + GUIStamp.PAGE_OPT_FIRST, I18N.message("firstpage"));
		pageOptions.put("" + GUIStamp.PAGE_OPT_LAST, I18N.message("lastpage"));
		pageOptions.put("" + GUIStamp.PAGE_OPT_SEL, I18N.message("selection"));
		pageOption.setValueMap(pageOptions);
		pageOption.setValue("" + stamp.getPageOption());
		pageOption.setWrap(false);
		pageOption.setEndRow(true);
		pageOption.addChangedHandler(new ChangedHandler() {

			@Override
			public void onChanged(ChangedEvent event) {
				if (event.getValue().equals("" + GUIStamp.PAGE_OPT_SEL))
					pageSelection.show();
				else
					pageSelection.hide();
				if (changedHandler != null)
					changedHandler.onChanged(null);
			}
		});

		SpinnerItem opacity = ItemFactory.newSpinnerItem("opacity", "opacity", stamp.getOpacity(), 1, 100);
		opacity.addChangedHandler(changedHandler);

		final SpinnerItem size = ItemFactory.newSpinnerItem("size", "size", stamp.getSize(), 1, 1000);
		size.addChangedHandler(changedHandler);

		TextItem font = ItemFactory.newTextItem("font", "font", stamp.getFont());
		font.addChangedHandler(changedHandler);
		font.setHint(I18N.message("fontpathhint"));
		font.setWidth(300);

		form1.setItems(name, type, pageOption, pageSelection, exprx, rotation, expry, opacity, exprw, description,
				exprh);

		form2.setItems(text, color, size, font, barcodeFormat, imageWidth, imageHeight, barcodeLabel);

		/*
		 * For the spinners we need to manually update the VM or the widget will
		 * NOT be refreshed (unbelievable but that is)
		 */
		vm.setValue("rotation", stamp.getRotation());
		vm.setValue("opacity", stamp.getOpacity());
		vm.setValue("size", stamp.getSize());

		formsContainer.setMembers(form1, form2, image);

		ChangedHandler typeChangedhandler = new ChangedHandler() {
			@Override
			public void onChanged(ChangedEvent event) {
				stamp.setType(Integer.parseInt(type.getValue().toString()));
				refresh();
			}
		};
		type.addChangedHandler(typeChangedhandler);
		type.addChangedHandler(changedHandler);

		if (type.getValue().toString().equals("" + GUIStamp.TYPE_IMAGE)) {
			text.hide();
			color.hide();
			size.hide();
			font.hide();
			barcodeFormat.hide();
			imageWidth.hide();
			imageHeight.hide();
			barcodeLabel.hide();
			if (stamp.getId() != 0L)
				image.show();
			exprh.show();
			refreshStampImage();
		} else {
			text.show();
			color.show();
			exprh.show();
			image.hide();

			if (type.getValue().toString().equals("" + GUIStamp.TYPE_BARCODE)) {
				barcodeFormat.show();
				imageWidth.show();
				imageHeight.show();
				barcodeLabel.show();
				size.hide();
				font.hide();
				color.hide();
			} else if (type.getValue().toString().equals("" + GUIStamp.TYPE_HTML)) {
				barcodeFormat.hide();
				barcodeLabel.hide();
				imageWidth.hide();
				imageHeight.hide();
				size.hide();
				color.hide();
				font.hide();
			} else {
				barcodeFormat.hide();
				barcodeLabel.hide();
				imageWidth.hide();
				imageHeight.hide();
				exprh.hide();
				size.show();
				font.show();
				color.show();
			}
		}
	}

	void refreshStampImage() {
		if (image != null && formsContainer.contains(image)) {
			formsContainer.removeMember(image);
			image.destroy();
		}

		if (stamp.getId() == 0L)
			return;

		image = new VLayout();
		image.setAlign(VerticalAlignment.TOP);
		image.setMargin(1);
		image.setMembersMargin(2);

		String html = "<img border='0' alt='' title='' src='" + stampImageUrl(stamp.getId())
				+ "' style='float:body; max-width:300px' align='body' />";
		HTMLFlow img = new HTMLFlow(html);
		img.setWidth100();
		img.setHeight(getHeight() - 60);

		IButton uploadStamp = new IButton(I18N.message("uploadstamp"));
		uploadStamp.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				StampUploader uploader = new StampUploader(stamp.getId(), StampProperties.this);
				uploader.show();
			}
		});
		uploadStamp.setDisabled(stamp.getId() == 0L);

		image.setMembers(uploadStamp, img);
		formsContainer.addMember(image);
	}

	static String stampImageUrl(long stampId) {
		return Util.contextPath() + (!Util.contextPath().endsWith("/") ? "/" : "") + "stampimage/" + stampId
				+ "?random=" + new Date().getTime();
	}

	boolean validate() {
		vm.validate();
		if (!vm.hasErrors()) {
			stamp.setName(vm.getValueAsString("name"));
			stamp.setType(Integer.parseInt(vm.getValueAsString("type")));
			stamp.setExprX(vm.getValueAsString("exprx"));
			stamp.setExprY(vm.getValueAsString("expry"));
			stamp.setExprW(vm.getValueAsString("exprw"));
			stamp.setExprH(vm.getValueAsString("exprh"));
			stamp.setRotation(Integer.parseInt(vm.getValueAsString("rotation")));
			stamp.setOpacity(Integer.parseInt(vm.getValueAsString("opacity")));
			stamp.setSize(Integer.parseInt(vm.getValueAsString("size")));
			stamp.setFont(vm.getValueAsString("font"));
			stamp.setPageOption(Integer.parseInt(vm.getValueAsString("pageOption")));
			stamp.setPageSelection(vm.getValueAsString("pageSelection"));

			if (vm.getValueAsString("imageWidth") != null)
				stamp.setImageWidth(Integer.parseInt(vm.getValueAsString("imageWidth")));
			if (vm.getValueAsString("imageHeight") != null)
				stamp.setImageHeight(Integer.parseInt(vm.getValueAsString("imageHeight")));

			stamp.setBarcodeLabel("yes".equals(vm.getValueAsString("barcodeLabel")) ? 1 : 0);
			stamp.setBarcodeFormat(vm.getValueAsString("barcodeFormat"));

			stamp.setDescription(vm.getValueAsString("description"));
			stamp.setColor(vm.getValueAsString("color"));
			stamp.setText(vm.getValueAsString("text"));
		}
		return !vm.hasErrors();
	}
}