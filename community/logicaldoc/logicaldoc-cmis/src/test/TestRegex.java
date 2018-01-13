package com.logicaldoc.cmis;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class TestRegex {
	
	public static void main(String[] args) {
		//accountCoder();
		accountCoder03();
	}
	
	private static void accountCoder03() {
		String patternText = "%.*%";
		String targetText = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document  WHERE cmis:name LIKE '%flexspaces%'";
		
		boolean success = false;
		
		Pattern pattern = Pattern.compile(patternText);
		Matcher matcher = pattern.matcher(targetText);
		//success = matcher.matches();
		
		success = matcher.find();
		System.out.println("success: " + success);
		String xxx = matcher.group();
		System.out.println("xxx: " + xxx);
		
		//System.out.println("success: " + success);
	}		
	
	private static void accountCoder02() {
		String patternText = "\\*.*\\*";
		String targetText = "SELECT cmis:objectId,cmis:name,cmis:lastModifiedBy,cmis:lastModificationDate,cmis:baseTypeId,cmis:contentStreamLength,cmis:versionSeriesId,cmis:contentStreamMimeType FROM cmis:document  WHERE CONTAINS('~cmis:name:\'*wiki*\'')";
		
		boolean success = false;
		
		Pattern pattern = Pattern.compile(patternText);
		Matcher matcher = pattern.matcher(targetText);
		//success = matcher.matches();
		
		success = matcher.find();
		System.out.println("success: " + success);
		String xxx = matcher.group();
		System.out.println("xxx: " + xxx);
		
		//System.out.println("success: " + success);
	}	

	private static void accountCoder() {
		String patternText = "\\d{3}-\\d{6}";
		String targetText = "010-020046";
		
		boolean success = false;
		
		Pattern pattern = Pattern.compile(patternText);
		Matcher matcher = pattern.matcher(targetText);
		success = matcher.matches();
		
		System.out.println("success: " + success);
	}
}
