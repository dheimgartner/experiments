import logging

def test_logger():
  for i in range(20):
    print("print {}".format(i))
    logging.warning("Warning {}".format(i))
    logging.info("Info {}".format(i))
