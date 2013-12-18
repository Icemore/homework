#include "evaluator.h"

const std::map<action::action_type, evaluator::executor> evaluator::execMethods = evaluator::initExecMethods();