import requests
import xml.etree.ElementTree as ET

def indent(elem, level=0):
    i = "\n" + level*"  "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = i + "  "
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
        for elem in elem:
            indent(elem, level+1)
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
    else:
        if level and (not elem.tail or not elem.tail.strip()):
            elem.tail = i

url="http://rss.cnn.com/rss/edition_tennis?format=xml"
xml=ET.fromstring(requests.get(url).text)
indent(xml)
print ET.tostring(xml)