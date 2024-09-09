
#1st product
#API_KEY hidden
from sec_api import XbrlApi
xbrlApi = XbrlApi(API_KEY)
from sec_edgar_downloader import Downloader as SecEdgarDownloader
from sec_downloader.download_storage import DownloadStorage
from sec_downloader import Downloader
from langchain_community.embeddings import GPT4AllEmbeddings
from langchain_community.vectorstores import Chroma
from langchain_text_splitters import CharacterTextSplitter
from langchain_openai.chat_models import ChatOpenAI
from langchain_openai.embeddings import OpenAIEmbeddings
from sec_downloader.types import RequestedFilings
import openai
from langchain_community.document_loaders import TextLoader
import pandas as pd
from sec_api import ExtractorApi
import os
from dotenv import load_dotenv
from langchain.prompts import ChatPromptTemplate
from langchain.text_splitter import RecursiveCharacterTextSplitter
from langchain_core.output_parsers import StrOutputParser
from langchain_core.runnables import RunnableParallel, RunnablePassthrough
from langchain_community.embeddings.sentence_transformer import (
        SentenceTransformerEmbeddings,
    )

# convert XBRL-JSON of income statement to pandas dataframe
def get_income_statement(xbrl_json):
    income_statement_store = {}

    # iterate over each US GAAP item in the income statement
    for usGaapItem in xbrl_json['StatementsOfIncome']:
        values = []
        indicies = []

        for fact in xbrl_json['StatementsOfIncome'][usGaapItem]:
            # only consider items without segment. not required for our analysis.
            if 'segment' not in fact:
                index = fact['period']['startDate'] + '-' + fact['period']['endDate']
                # ensure no index duplicates are created
                if index not in indicies:
                    values.append(fact['value'])
                    indicies.append(index)                    

        income_statement_store[usGaapItem] = pd.Series(values, index=indicies) 

    income_statement = pd.DataFrame(income_statement_store)
    # switch columns and rows so that US GAAP items are rows and each column header represents a date range
    return income_statement.T 







from flask import Flask, request

app = Flask(__name__)

@app.route('/my-route')
def my_route():
    Ticker = request.args.get('Ticker', default = " ", type = str)
  
    dl = Downloader("anything", "dellacm@usi.ch")
    metadatas = dl.get_filing_metadatas(
        RequestedFilings(ticker_or_cik=Ticker, form_type="10-K", limit=1)
    )
    # Check if metadatas list is not empty and print the primary document URL
    if metadatas:
        primary_doc_url = metadatas[0].primary_doc_url
        print("Primary Document URL:", primary_doc_url)
    else:
        print("No filings found.")
    # Now you can use `primary_doc_url` to download the filing, decode it, and print a snippet
    primary_doc_url
    url_10k = f"{primary_doc_url}"
    xbrl_json = xbrlApi.xbrl_to_json(htm_url=url_10k)
    
    income_statement = get_income_statement(xbrl_json)
    
    return f"""
            <head>
            <style>
                table,
                th,
                td {{
                font-size: 10pt;
                border: 1px solid black;
                border-collapse: collapse;
                }}
                th,
                td {{
                padding: 5px;
                }}
                tr:nth-child(even) {{
                background-color: #f2f2f2;
                }}
            </style>
            </head>
            {income_statement.to_html()}
            """



#2nd product

#API_KEY hidden

def pprint(text, line_length=100):
    words = text.split(' ')
    lines = []
    current_line = ''
    for word in words:
        if len(current_line + ' ' + word) <= line_length:
            current_line += ' ' + word
    else:
        lines.append(current_line.strip())
        current_line = word
    if current_line:
        lines.append(current_line.strip())
    print('\n'.join(lines))

extractorApi = ExtractorApi(API_KEY)


@app.route('/ask-llm')
def ask_llm():
    Question = request.args.get('Question', default = " ", type = str)
    
    Ticker = request.args.get('Ticker', default = " ", type = str)
    
    dl = Downloader("anything", "dellacm@usi.ch")
    metadatas = dl.get_filing_metadatas(
        RequestedFilings(ticker_or_cik=Ticker, form_type="10-K", limit=1)
    )
    # Check if metadatas list is not empty and print the primary document URL
    if metadatas:
        primary_doc_url = metadatas[0].primary_doc_url
        print("Primary Document URL:", primary_doc_url)
    else:
        print("No filings found.")
    # Now you can use `primary_doc_url` to download the filing, decode it, and print a snippet
    primary_doc_url
    url_10k = f"{primary_doc_url}"
    filing_10_k_url = url_10k
    item_1_text    = extractorApi.get_section(filing_10_k_url, '1', 'text')
    item_1_a_text  = extractorApi.get_section(filing_10_k_url, '1A', 'text')
    item_1_b_text  = extractorApi.get_section(filing_10_k_url, '1B', 'text')
    item_2_text    = extractorApi.get_section(filing_10_k_url, '2', 'text')
    item_3_text    = extractorApi.get_section(filing_10_k_url, '3', 'text')
    item_4_text    = extractorApi.get_section(filing_10_k_url, '4', 'text')
    item_5_text    = extractorApi.get_section(filing_10_k_url, '5', 'text')
    item_6_text    = extractorApi.get_section(filing_10_k_url, '6', 'text')
    item_7_text    = extractorApi.get_section(filing_10_k_url, '7', 'text')
    item_7_a_text  = extractorApi.get_section(filing_10_k_url, '7A', 'text')
    item_8_text    = extractorApi.get_section(filing_10_k_url, '8', 'text')
    item_9_text    = extractorApi.get_section(filing_10_k_url, '9', 'text')
    item_9_a_text  = extractorApi.get_section(filing_10_k_url, '9A', 'text')
    item_9_b_text  = extractorApi.get_section(filing_10_k_url, '9B', 'text')
    item_10_text   = extractorApi.get_section(filing_10_k_url, '10', 'text')
    item_11_text   = extractorApi.get_section(filing_10_k_url, '11', 'text')
    item_12_text   = extractorApi.get_section(filing_10_k_url, '12', 'text')
    item_13_text   = extractorApi.get_section(filing_10_k_url, '13', 'text')
    item_14_text   = extractorApi.get_section(filing_10_k_url, '14', 'text')
    item_15_text   = extractorApi.get_section(filing_10_k_url, '15', 'text')
    texts = [
        item_1_text, item_1_a_text, item_1_b_text, item_2_text, item_3_text,
        item_4_text, item_5_text, item_6_text, item_7_text, item_7_a_text,
        item_8_text, item_9_text, item_9_a_text, item_9_b_text, item_10_text,
        item_11_text, item_12_text, item_13_text, item_14_text, item_15_text
    ]

    # Join all items in the list into a single string
    combined_text = ''.join(texts)
    with open("textanalyse.txt", "w", encoding="utf-8") as file:
        # Scrive la stringa nel file
        file.write(combined_text)

    loader = TextLoader("textanalyse.txt", encoding = 'UTF-8')
    text_documents = loader.load()
    text_documents  
    
        #API_KEY hidden
    # Define the path to the .env file
    env_file_path = './.env'

    # Write the API Key to the .env file
    with open(env_file_path, 'w', encoding='utf-8') as f:
        f.write(f"OPENAI_API_KEY={api_key}\n")

    print(f".env file created at {os.path.abspath(env_file_path)}")
    #python-dotenv


    load_dotenv()
 
    OPENAI_API_KEY = os.getenv("OPENAI_API_KEY")
    model="gpt-4-turbo"
    model = ChatOpenAI(openai_api_key=OPENAI_API_KEY, model=model)
    embeddings = OpenAIEmbeddings()


    parser = StrOutputParser()
    template = """
    "you need to understand the documents".

    Context: {context}

    Question: {question}
    """
    prompt = ChatPromptTemplate.from_template(template)
   

    # Assuming you have the 'HF_TOKEN' value
    hf_token = "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"
    os.environ["HUGGINGFACEHUB_API_TOKEN"] = hf_token

    # Now, you can use the token from environment variables
    token = os.getenv("HUGGINGFACEHUB_API_TOKEN")
    #----------SPLITTING THE TEXT--------
    

    text_splitter = RecursiveCharacterTextSplitter(chunk_size=10000, chunk_overlap=20)
    documents = text_splitter.split_documents(text_documents)

    #----------finding relevant chunks--------


    # create the open-source embedding function
    embeddings= SentenceTransformerEmbeddings(model_name="all-MiniLM-L12-v2")
    #---------vector store--------
    db = Chroma.from_documents(documents, embeddings)

    
    chain = (
        {"context": db.as_retriever(), "question": RunnablePassthrough()}
        | prompt
        | model
        | parser
    )
    return chain.invoke(Question)
