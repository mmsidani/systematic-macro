/// ==========================================================
/// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT 
/// WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED,    
/// INCLUDING BUT NOT LIMITED TO THE IMPLIED WARRANTIES   
/// OF MERCHANTABILITY AND/OR FITNESS FOR A  PARTICULAR   
/// PURPOSE.								
/// ==========================================================

import java.io.*;
import java.util.*;

import com.bloomberglp.blpapi.Element;
import com.bloomberglp.blpapi.Event;
import com.bloomberglp.blpapi.Message;
import com.bloomberglp.blpapi.MessageIterator;
import com.bloomberglp.blpapi.Request;
import com.bloomberglp.blpapi.Service;
import com.bloomberglp.blpapi.Session;
import com.bloomberglp.blpapi.SessionOptions;
import com.bloomberglp.blpapi.Datetime;
import com.bloomberglp.blpapi.Schema.Datatype;

public class BloombergData {

	// our settings
	private static final String tickerFieldLinker = "___";

	// if bloomberg changed their minds and renamed "securityData" "security_data", e.g., we would only have to change the following line. Similarly for the other static variables below
	private static final String SECURITY_DATA = "securityData";
	private static final String SECURITY_NAME = "security";
	private static final String FIELD_DATA = "fieldData";
	private static final String DATE = "date";
	private static final String RESPONSE_ERROR = "responseError";
	
	// we cannot submit requests with any number of requests. by trial and error, we found that submitting 5000 securities in one request works
	private static final int MAX_SECURITIES_IN_ONE_REQUEST = 5000;

	private static ArrayList<ArrayList<String>> readReferenceData(String refDataFile) {
		// read the file that gives BB tickers, BB fields, sa2 names, and divisors. this file determines what we request from bb.

		try{
			BufferedReader br = new BufferedReader(new FileReader(refDataFile));

			ArrayList<String> bbTickers = new ArrayList<String>();
			ArrayList<String> bbFields = new ArrayList<String>(); 
			ArrayList<String> sa2IDs = new ArrayList<String>();
			ArrayList<String> divisors = new ArrayList<String>();
			String newLine;
			while( (newLine = br.readLine()) != null ) {
				newLine = newLine.trim();
				if(newLine.startsWith("#")) continue; // we allow for comments

				String[] fields = newLine.split(",");
				bbTickers.add(fields[0]);
				bbFields.add(fields[1]);
				sa2IDs.add(fields[2]);
				divisors.add(fields[3]);
			}

			br.close();

			ArrayList<ArrayList<String>> ret = new ArrayList<ArrayList<String>>();
			ret.add(bbTickers);
			ret.add(bbFields);
			ret.add(sa2IDs);
			ret.add(divisors);

			return ret;

		} catch(IOException e){
			System.err.println("ERROR in IO in BloombergData.readReferenceData():" + e);
			return(null);
		} catch(Exception e){
			System.err.println("ERROR in BloombergData.readReferenceData():" + e);
			return(null);
		}
	}
	
	private static BufferedWriter openOutput(String outputFileName ){
		try{
			return( new BufferedWriter(new FileWriter(outputFileName)) );
		} catch( IOException e){
			System.err.println("ERROR in BloombergData.writeOutput(): " + e);
			return( null );
		}
	}
	
	private static BufferedWriter writeOutput(BufferedWriter bw, String[] output ){
		// save intermediate output to a file which will then be read by the R code for further processing and uploading to the database

		if(output == null || output.length == 0 ){ 
			System.out.println("WARNING in BloombergData.writeOutput(): No data was returned.");
			return( bw );
		}
		
		try{
			for (int i = 0 ; i < output.length; ++i){
				bw.write( output[i] );
				bw.newLine();
			}
			bw.flush();
			return( bw );
		} catch( IOException e){
			System.err.println("ERROR in BloombergData.writeOutput(): " + e);
			return( null );
		}
	}
	
	private static void closeOutput( BufferedWriter bw ){
		try{
			bw.close();
		} catch( IOException e){
			System.err.println("ERROR in BloombergData.writeOutput(): " + e);
		}
	}

	private static Session getSession() {
		// infrastructure stuff. taken from the bb examples

		String serverHost = "localhost";
		int serverPort = 8194;

		SessionOptions sessionOptions = new SessionOptions();
		sessionOptions.setServerHost(serverHost);
		sessionOptions.setServerPort(serverPort);

		Session session = new Session(sessionOptions);
		try{
			if (!session.start()) {
				System.err.println("Failed to start session.");
				return(null);
			}
			if (!session.openService("//blp/refdata")) {
				System.err.println("Failed to open //blp/refdata");
				return(null);
			}
		} catch(Exception e){

		}
		return(session);
	}

	private static LinkedHashMap<String, LinkedHashSet<String>> mapFieldsToTickers(ArrayList<String> bbTickers, ArrayList<String> fields){
		// we want to submit one field per request to bb. so map this field to all names for which we want this particular field and make sure no duplicates (that's why we use HashSet instead of ArrayList)

		LinkedHashMap<String,LinkedHashSet<String>> ret = new LinkedHashMap<String,LinkedHashSet<String>>();
		LinkedHashSet<String> uniqueFields = new LinkedHashSet<String>(fields);
		Iterator<String> iter = uniqueFields.iterator();
		while(iter.hasNext()){
			String field = (String) iter.next();
			ret.put(field, new LinkedHashSet<String>());
		}

		for (int i = 0; i < bbTickers.size(); ++i){
			ret.get(fields.get(i)).add(bbTickers.get(i));
		}

		return(ret);
	}

	private static LinkedHashMap<String, ArrayList<String> > mapBBToSA2( ArrayList<String> bbTickers, ArrayList<String> fields, ArrayList<String> sa2Ids) {
		// once we get our bb data, we want to rename it. so here map the tickers+fields to their sa2 names and remember that one pair (bb_ticker, field) could map to more than just one sa2 name

		LinkedHashMap<String,  ArrayList<String>> ret = new LinkedHashMap<String,  ArrayList<String>>();
		for (int i=0; i < bbTickers.size(); ++i){
			String key = new String(bbTickers.get(i) + tickerFieldLinker + fields.get(i));
			if( !ret.containsKey( key )){
				ret.put( key, new ArrayList<String>());
			}
			ret.get(key).add(sa2Ids.get(i));
		}

		return ret;
	}

	private static LinkedHashMap<String, Double> mapFieldToDivisor(ArrayList<String> bbTickers, ArrayList<String> fields, ArrayList<String> divisors){
		// some fields need to be divided by a number specified in the reference data file that we read earlier

		LinkedHashMap<String, Double> ret= new LinkedHashMap<String, Double>();
		for(int i = 0; i < fields.size(); ++i){
			try{
				String key = new String(bbTickers.get(i) + tickerFieldLinker + fields.get(i));
				ret.put(key, new Double(divisors.get(i)));
			} catch (Exception e){
				System.err.println("ERROR in BloombergData.mapFieldToDivisor(): "+e);
			}
		}

		return(ret);
	}

	private static ArrayList<String> processFields(String desiredField, LinkedHashMap<String,ArrayList<String>> bbTickerField2SA2, LinkedHashMap<String,Double> divisors, Element securityData, String requestType, String overrideDate ) {

		// 1 message per security
		String ticker = securityData.getElementAsString(SECURITY_NAME);
		Element fieldData = securityData.getElement(FIELD_DATA);

		String bbField = desiredField.toUpperCase();
		String bbTickerField = ticker.toString() + tickerFieldLinker + desiredField;
		ArrayList<String> sa2Tickers = bbTickerField2SA2.get(bbTickerField);
		double divisor = divisors.get( bbTickerField );

		ArrayList<String> ret = new ArrayList<String>();
		if (fieldData.numValues() > 0) {
			int numValues = fieldData.numValues();
			try {

				if( requestType.equals("HistoricalDataRequest")){
					for(int k = 0; k < numValues; k++) {
						Element element = fieldData.getValueAsElement(k);
						Datetime date = element.getElementAsDatetime(DATE);
						if(element.hasElement(bbField)) {
							for (int i=0; i< sa2Tickers.size(); ++i ){
								String value = date.toString()+ "," + sa2Tickers.get(i) + "," + ( (divisor == 1.0 || divisor == 0.0) ? element.getElementAsString(bbField) : element.getElementAsFloat64(bbField) / divisor );
								ret.add( value );
							}
						} 
					}
				} else if( requestType.equals("ReferenceDataRequest")) {
					String date = overrideDate.substring(0, 4) + "-" + overrideDate.substring(4, 6) + "-" + overrideDate.substring(6, 8);
					for(int k = 0; k < numValues; k++) {
						Element fieldDataElement = fieldData.getElement(k);
						if( fieldDataElement.datatype() == Datatype.SEQUENCE ){
							// IMPORTANT Note: the logic here was written with INDEX_MWEIGHT_HIST as the bb field in mind. in particular, we get 1 FLOAT64 field and we say the divisor field from the input file applies to it. Should we start asking for a different bb field that returns more than 1 FLOAT64 and should different divisors be needed for these then this piece of code will have to be tucked under an if(bbField=="INDEX_MWEIGHT_HIST") and an else if for the new bb field created	
							for (int l = 0; l < fieldDataElement.numValues(); ++l){
								Element field = fieldDataElement.getValueAsElement(l);
								for (int i=0; i< sa2Tickers.size(); ++i ){
									String value = date.toString()+ "," + sa2Tickers.get(i);
									for( int f = 0; f < field.numElements(); ++f ){
										Element member = field.getElement(f);
										double div = member.datatype() == Datatype.FLOAT64 ? divisor : 0.0;
										if( div == 1.0 || div == 0.0 ){
											value += "," + member.getValueAsString();
										} else {
											value += "," + member.getValueAsFloat64() / div;
										}
									}
									ret.add(value);
								}
							}
						} else if ( fieldDataElement.datatype() == Datatype.FLOAT64 ) {
							for (int i=0; i< sa2Tickers.size(); ++i ){
								String value = date.toString()+ "," + sa2Tickers.get(i) + "," + fieldDataElement.getValueAsFloat64() / divisor;
								ret.add(value);
							}
						} else if ( fieldDataElement.datatype() == Datatype.STRING ) {
							for (int i=0; i< sa2Tickers.size(); ++i ){
								String value = date.toString()+ "," + sa2Tickers.get(i) + "," + fieldDataElement.getValueAsString();
								ret.add(value);
							}
						} else {
							throw( new Exception( "ERROR in BloombergData.processFields(): The data type returned by the ReferenceDataRequest is not supported."));
						}
					}
				} 
			} catch(Exception e) {
				System.err.println(e.toString());
			}
		}

		return(ret);
	}
	
	private static Request buildRequest(String field, LinkedHashSet<String> bbTickers, Session session, String startDate, String endDate, String frequency, String requestType, boolean fillNonTradingDays) {
		Service service = session.getService("//blp/refdata");
		Request request = service.createRequest(requestType);
		Element secs = request.getElement("securities");
		Iterator<String> iter = bbTickers.iterator();
		while(iter.hasNext()){
			secs.appendValue( (String) iter.next() );
		}

		Element fields = request.getElement("fields");
		fields.appendValue(field);

		if( requestType.equals("HistoricalDataRequest") ) {
			request.set("periodicityAdjustment", "CALENDAR");
			request.set("periodicitySelection", frequency);
			request.set("startDate", startDate);
			request.set("endDate", endDate);
			if( fillNonTradingDays ){
				request.set("nonTradingDayFillOption", "NON_TRADING_WEEKDAYS");
				request.set("nonTradingDayFillMethod", "PREVIOUS_VALUE");
			}
		} else if ( requestType.equals("ReferenceDataRequest") ){
			Element overrides = request.getElement("overrides");
			Element override = overrides.appendElement();
			override.setElement("fieldId", "END_DATE_OVERRIDE");
			override.setElement("value", endDate );
		} else {
			System.err.println("ERROR in buildRequest(): request type "+ requestType+" is not supported.");
			return( null );
		}

		return(request);
	}

	public static void getData(String startDate, String endDate, ArrayList<String> bbTickers, ArrayList<String> fields, ArrayList<String> sa2Ids, ArrayList<String> divisors, String freq, Session session, String requestType, boolean fillNonTradingDays, String outputFile) {
		LinkedHashMap<String,LinkedHashSet<String>> fieldToTickers = mapFieldsToTickers(bbTickers, fields);
		LinkedHashMap<String, ArrayList<String>> bbTickerField2SA2 = mapBBToSA2(bbTickers, fields, sa2Ids);
		// map fields to the corresponding divisors and also convert divisors to double's
		LinkedHashMap<String,Double> field2Divisor = mapFieldToDivisor(bbTickers, fields, divisors);

		BufferedWriter bw = openOutput( outputFile );

		Iterator<String> iter = fieldToTickers.keySet().iterator();
		while(iter.hasNext()){
			String field = (String) iter.next();
			Iterator<String> iterSecs = fieldToTickers.get(field).iterator();
			while(iterSecs.hasNext()){
				LinkedHashSet<String> secs = new LinkedHashSet<String>();
				int counter = 0;
				while(iterSecs.hasNext() && counter < MAX_SECURITIES_IN_ONE_REQUEST){
					secs.add( (String) iterSecs.next() );
					++counter;
				}

				try{
					// 1 field per request: simpler this way.
					Request request = buildRequest(field, secs, session, startDate, endDate, freq, requestType, fillNonTradingDays );
					session.sendRequest(request, null);
					while (true) {
						Event event = session.nextEvent();
						MessageIterator msgIter = event.messageIterator();

						while (msgIter.hasNext()) {
							Message msg = msgIter.next();
							if (msg.hasElement(RESPONSE_ERROR)) {
								System.err.println("ERROR in getData(): " + msg.getElement(RESPONSE_ERROR));
								continue;
							}
							if(msg.hasElement(SECURITY_DATA)){
								Element securityData = msg.getElement(SECURITY_DATA);
								if( requestType.equals("ReferenceDataRequest" )){
									for (int i = 0; i < securityData.numValues(); ++i){
										Element oneSecurityData = securityData.getValueAsElement(i);
										ArrayList<String> processed = processFields( field, bbTickerField2SA2, field2Divisor, oneSecurityData, requestType, endDate  );
										if( processed.size() == 0 ) System.out.print( oneSecurityData.getElementAsString(SECURITY_NAME) + " . " + field + ": " );
										bw = writeOutput( bw, processed.size() == 0 ? null : processed.toArray( new String[1]) );
									}
								} else if ( requestType.equals("HistoricalDataRequest") ){
									ArrayList<String> processed = processFields( field, bbTickerField2SA2, field2Divisor, securityData, requestType, null);
									if( processed.size() == 0 ) System.out.print( securityData.getElementAsString(SECURITY_NAME) + " . " + field + ": " );
									bw = writeOutput( bw, processed.size() == 0 ? null : processed.toArray( new String[1]) );
								}
							}
						}
						if (event.eventType() == Event.EventType.RESPONSE) {
							break;
						}
					}

				} catch(Exception e){
					System.err.println(e);
					closeOutput( bw );
				}
			}
		}

		closeOutput( bw );
	}

	public static void main (String[] args) {
		//public static void main(String[] args) throws Exception
		/* 
		 * args[0]: start date
		 * args[1]: end date
		 * args[2]: frequency of data
		 * args[3]: reference data file name (bloomberg and SA2 id's and fields)
		 * args[4]: output file
		 * args[5]: request type, "HistoricalDataRequest", "ReferenceDataRequest"
		 * args[6]: do we fill non-trading days with previous values? "true" or "false" (NOTE: only needed for "HistoricalDataRequest" )
		 */

		Session session = getSession();

		String startDate = args[0];
		String endDate = args[1];
		String dataFrequency = args[2];
		ArrayList<ArrayList<String>> referenceData = readReferenceData(args[3]);
		String outputFile = args[4];
		String requestType = args[5];
		boolean fillNonTradingDays = true;
		if (requestType.equals("HistoricalDataRequest") ) {
			// we only get the 6th argument with "HistoricalDataRequest"
			fillNonTradingDays = Boolean.parseBoolean( args[6].toLowerCase() );
		}

		// a necessary but NOT sufficient condition for the dates to be in the right format
		if( startDate.contains("-") || endDate.contains("-")){
			System.err.println("ERROR in BloombergData.main(): one or both dates are in the wrong format: "+startDate+", "+endDate );
			return ;
		}

		getData(startDate, endDate, referenceData.get(0), referenceData.get(1), referenceData.get(2), referenceData.get(3), dataFrequency, session, requestType, fillNonTradingDays, outputFile );

	}


}
