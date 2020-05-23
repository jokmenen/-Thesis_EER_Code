#!/usr/bin/env python
# coding: utf-8

# Make an object which can be initiallized (to load all the nessecary models etc so it doesnt
# have to be done each time). Make individual class detector methods.
# Make a predict class pipeline that goes through the flowchart,
# initiating each class detector when needed.

import math
from pytorch_pretrained_bert import OpenAIGPTTokenizer, OpenAIGPTModel, OpenAIGPTLMHeadModel

class Explanation_error_detector:
    
    def __init__(perplexity_threshold=137):
        ### Lang Model:
        # Load Language Model
        # Load pre-trained model (weights)
        model = OpenAIGPTLMHeadModel.from_pretrained('openai-gpt')
        model.eval()
        # Load pre-trained model tokenizer (vocabulary)
        tokenizer = OpenAIGPTTokenizer.from_pretrained('openai-gpt')
        
        ### For clarity error:
        self.perplexity_threshold = perplexity_threshold
        
    def perplexity_score(sentence):
        tokenize_input = tokenizer.tokenize(sentence)
        tensor_input = torch.tensor([tokenizer.convert_tokens_to_ids(tokenize_input)])
        loss=model(tensor_input, lm_labels=tensor_input)
        return math.exp(loss)
    
    def detect_clarity_error(explanation):
        if self.perplexity_score(explanation)>self.perplexity_threshold:
            return True
        else:
            return False
        

